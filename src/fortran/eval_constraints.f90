! MIT License

module eval_constraints

  !-------------------------------------------------------------------------
  ! evaluations of airfoil geometry constraints
  !-------------------------------------------------------------------------
   
  use os_util
  use string_util,            only : stri, strf
  use airfoil_base,           only: airfoil_type, side_airfoil_type, is_hh_based
  use airfoil_geometry,       only: max_curvature_at_te, te_angle_top, te_angle_bot
  use eval_commons
  use math_util,              only : sort_vector_descend, diff_1D, round

  implicit none
  private 

  public :: geo_constraints_type
  public :: curv_constraints_type
  public :: curv_side_constraints_type

  public :: penalty_curv, penalty_geo, penalty_curv_of_side
  
  public :: penalty_stats_init, print_penalty_stats, print_penalty_stats_table
  public :: print_penalty_stats_avg, print_penalty_stats_trigger
  public :: print_penalty_stats_last
  public :: penalty_reversals
  public :: penalty_min_thickness, penalty_max_thickness
  public :: penalty_min_camber, penalty_max_camber
  public :: penalty_min_te_angle, penalty_min_te_top_angle, penalty_max_te_bot_angle
  public :: penalty_min_thickness_at_x
  public :: penalty_te_curv, penalty_le_curv
  public :: penalty_bumpiness
  public :: penalty_d4_bspline

  public :: has_penalty 
  public :: PEN_MIN_THICKNESS, PEN_MAX_THICKNESS, PEN_MIN_CAMBER, PEN_MAX_CAMBER
  public :: PEN_MIN_TE_ANGLE, PEN_MIN_TE_TOP_ANGLE, PEN_MAX_TE_BOT_ANGLE, PEN_MIN_THICKNESS_AT_X
  public :: PEN_LE_CURV_MONOTON, PEN_TE_CURV, PEN_CURV_REVERSALS, PEN_CURV_BUMPS, PEN_D4

  ! constraint specifications for geometry and curvature

  double precision, parameter, public :: PEN_GEO_SCALE    = 10d0    ! scale factor for geometry penalties
  double precision, parameter, public :: PEN_CURVE_SCALE  = 100d0   ! scale factor for curvature penalties
  integer, parameter                  :: PENALTY_DECIMALS  = 6


  ! --- private, static ---------------------------------------------------

  ! penalty ids

  integer, parameter :: PEN_MIN_THICKNESS      = 1
  integer, parameter :: PEN_MAX_THICKNESS      = 2
  integer, parameter :: PEN_MIN_CAMBER         = 3
  integer, parameter :: PEN_MAX_CAMBER         = 4
  integer, parameter :: PEN_MIN_TE_ANGLE       = 5
  integer, parameter :: PEN_MIN_TE_TOP_ANGLE   = 6
  integer, parameter :: PEN_MAX_TE_BOT_ANGLE   = 7
  integer, parameter :: PEN_MIN_THICKNESS_AT_X = 8
  integer, parameter :: PEN_LE_CURV            = 9
  integer, parameter :: PEN_LE_CURV_MONOTON    = 10
  integer, parameter :: PEN_TE_CURV            = 11
  integer, parameter :: PEN_CURV_REVERSALS     = 12
  integer, parameter :: PEN_CURV_BUMPS         = 13
  integer, parameter :: PEN_D4                 = 14
  integer, parameter :: N_PENALTY_TYPES        = 14



  type penalty_stat_type
    character (:), allocatable :: name
    integer          :: count                  ! number of violations of this type
    double precision :: last                   ! last penalty value of this type
    double precision :: trigger_value          ! last measured value associated with this penalty
    double precision :: total                  ! total penalty of this type 
    double precision :: average                ! average penalty of this type (Welford)
    double precision :: max                    ! max penalty of this type
    double precision :: M2, std                ! Welford accumulator and standard deviation
                                               ! Use CV = std/average to judge scaling quality:
                                               !   CV << 1 : penalty fires consistently  -> scale is well calibrated
                                               !   CV >> 1 : rare large spikes dominate  -> consider reducing scale
  end type

  type(penalty_stat_type), allocatable, save :: penalty_stats(:)


  contains


  !--------------------------------------------------------------------------------------


  function penalty_geo  (foil, geometry_constraints) result (penalty)

    !! evaluate absolute geometry constraint violation against the user limits.

    use airfoil_geometry, only : get_geometry

    type (airfoil_type), intent(in)         :: foil
    type (geo_constraints_type), intent(in) :: geometry_constraints
    double precision                        :: penalty
    double precision                        :: p_min_thickness, p_max_thickness
    double precision                        :: p_min_camber, p_max_camber
    double precision                        :: p_min_te_angle, p_min_te_top_angle, p_max_te_bot_angle
    double precision                        :: p_min_thickness_x
    double precision                        :: maxt, xmaxt, maxc, xmaxc
    logical                                 :: need_geometry

    type (geo_constraints_type) :: c_spec

    penalty = 0d0
    p_min_thickness = 0d0
    p_max_thickness = 0d0
    p_min_camber    = 0d0
    p_max_camber    = 0d0
    p_min_te_angle  = 0d0
    p_min_te_top_angle = 0d0
    p_max_te_bot_angle = 0d0
    p_min_thickness_x = 0d0

    c_spec = geometry_constraints

    if (.not. c_spec%check_geometry) return

    need_geometry = (c_spec%min_thickness /= NOT_DEF_D) .or. &
                    (c_spec%max_thickness /= NOT_DEF_D) .or. &
                    (c_spec%min_camber    /= NOT_DEF_D) .or. &
                    (c_spec%max_camber    /= NOT_DEF_D)

    if (need_geometry) then

      call get_geometry (foil, maxt, xmaxt, maxc, xmaxc)
      p_min_thickness = penalty_min_thickness (maxt, c_spec%min_thickness, scale=0.001d0)
      p_max_thickness = penalty_max_thickness (maxt, c_spec%max_thickness, scale=0.001d0)
      p_min_camber    = penalty_min_camber    (maxc, c_spec%min_camber,    scale=0.001d0)
      p_max_camber    = penalty_max_camber    (maxc, c_spec%max_camber,    scale=0.001d0)

    end if
  
    p_min_te_angle     = penalty_min_te_angle     (foil, c_spec%min_te_angle,     scale=0.001d0)
    p_min_te_top_angle = penalty_min_te_top_angle (foil, c_spec%min_te_top_angle, scale=0.001d0)
    p_max_te_bot_angle = penalty_max_te_bot_angle (foil, c_spec%max_te_bot_angle, scale=0.001d0)

    p_min_thickness_x = penalty_min_thickness_at_x (foil, c_spec%min_thickness_at_x%x, &
                            c_spec%min_thickness_at_x%y, scale=0.001d0)

    penalty = p_min_thickness + p_max_thickness + p_min_camber + p_max_camber + &
          p_min_te_angle + p_min_te_top_angle + p_max_te_bot_angle + p_min_thickness_x

    penalty = min (penalty * PEN_GEO_SCALE, 1d0)   ! scale up and cap to avoid extreme values

    penalty = round (penalty, PENALTY_DECIMALS)

  end function penalty_geo


  function penalty_min_thickness (thickness, min_thickness, scale) result (penalty)

    !! Penalize a maximum thickness below the minimum allowed thickness.

    double precision, intent(in) :: thickness
    double precision, intent(in) :: min_thickness
    double precision, intent(in) :: scale
    double precision             :: penalty

    penalty = 0d0

    if (min_thickness == NOT_DEF_D .or. thickness == NOT_DEF_D) return

    if (thickness < min_thickness) then
      penalty = (min_thickness - thickness) * 500d0 ! rougly about 2% less should be 1.0

      penalty = min (penalty, 2d0)    ! cap to avoid extreme values before scaling
      penalty = penalty ** 0.3        ! smaller values more severe
      penalty = penalty * scale
    end if

    call add_to_penalty_stats (PEN_MIN_THICKNESS, penalty, thickness)

  end function penalty_min_thickness



  function penalty_max_thickness (thickness, max_thickness, scale) result (penalty)

    !! Penalize a maximum thickness above the maximum allowed thickness.
    !! Returns max penalty = 1.0 for scale = 1.0  

    double precision, intent(in) :: thickness
    double precision, intent(in) :: max_thickness
    double precision, intent(in) :: scale
    double precision             :: penalty

    penalty = 0d0

    if (max_thickness == NOT_DEF_D .or. thickness == NOT_DEF_D)  return

    ! estimation of penalty for thickness above max:
    ! if max thickness is 0.1 (10%) then max penalty should be for thickness of 0.102 (10.2%), 
    ! this should be penalty = 1.0 - not scaled yet, 
    ! so penalty = (thickness - max_thickness) / (0.102 - 0.1) = (thickness - max_thickness) / 0.002

    if (thickness > max_thickness) then
      penalty = (thickness - max_thickness) * 500d0 ! rougly about 2% more should be 1.0
      penalty = min (penalty, 2d0)    ! cap to avoid extreme values before scaling
      penalty = penalty ** 0.3        ! smaller values more severe
      penalty = penalty * scale
    end if

    call add_to_penalty_stats (PEN_MAX_THICKNESS, penalty, thickness)

  end function penalty_max_thickness



  function penalty_min_camber (camber, min_camber, scale) result (penalty)

    !! Penalize a maximum camber below the minimum allowed camber.

    double precision, intent(in) :: camber
    double precision, intent(in) :: min_camber
    double precision, intent(in) :: scale
    double precision             :: penalty

    penalty = 0d0

    if (min_camber == NOT_DEF_D .or. camber == NOT_DEF_D) return

    ! see max_camber penalty for rationale of estimation of penalty for camber below min

    if (camber < min_camber) then
      penalty = (min_camber - camber) * 500d0 ! rougly about 2% less should be 1.0
      penalty = min (penalty, 2d0)    ! cap to avoid extreme values before scaling
      penalty = penalty ** 0.3        ! smaller values more severe
      penalty = penalty * scale
    end if

    call add_to_penalty_stats (PEN_MIN_CAMBER, penalty, camber)

  end function penalty_min_camber



  function penalty_max_camber (camber, max_camber, scale) result (penalty)

    !! Penalize a maximum camber above the maximum allowed camber.

    double precision, intent(in) :: camber
    double precision, intent(in) :: max_camber
    double precision, intent(in) :: scale
    double precision             :: penalty

    penalty = 0d0

    if (max_camber == NOT_DEF_D .or. camber == NOT_DEF_D) return

    ! estimation of penalty for camber above max:
    ! if max camber is 0.02 (2%) then max penalty should be for camber of 0.022 (2.2%), 
    ! this should be penalty = 1.0 - not scaled yet, 
    ! so penalty = (camber - max_camber) / (0.022 - 0.02) = (camber - max_camber) / 0.002

    if (camber > max_camber) then
      penalty = (camber - max_camber) * 500d0 ! rougly about 2% more should be 1.0
      penalty = min (penalty, 2d0)    ! cap to avoid extreme values before scaling
      penalty = penalty ** 0.3        ! smaller values more severe
      penalty = penalty * scale
    end if

    call add_to_penalty_stats (PEN_MAX_CAMBER, penalty, camber)

  end function penalty_max_camber



  function penalty_min_te_angle (foil, min_angle, scale) result (penalty)

    !! Penalize a trailing-edge angle below the minimum allowed angle.

    use airfoil_geometry, only : te_angle

    type (airfoil_type), intent(in) :: foil
    double precision, intent(in)    :: min_angle
    double precision, intent(in)    :: scale
    double precision                :: penalty

    double precision :: angle

    penalty = 0d0

    if (min_angle == NOT_DEF_D) return

    ! estimation of penalty for te angle below min:
    ! if min angle is 10 degrees then max penalty should be for angle of 11 degrees, 
    ! this should be penalty = 1.0 - not scaled yet, 
    ! so penalty = (min_angle - angle) / (11 - 10) = (min_angle - angle) / 1

    angle = te_angle (foil)
    if (angle < min_angle) then
      penalty = (min_angle - angle) * 1.0d0 ! rougly about 1° less should be 1.0
      penalty = min (penalty, 2d0)    ! cap to avoid extreme values before scaling
      penalty = penalty ** 0.5        ! smaller values more severe
      penalty = penalty * scale
    end if

    call add_to_penalty_stats (PEN_MIN_TE_ANGLE, penalty, angle)

  end function penalty_min_te_angle


  function penalty_min_thickness_at_x (foil, x_pos, min_thickness, scale) result (penalty)

    !! Penalize local thickness below minimum thickness at a given x-position.

    use airfoil_geometry, only : eval_y_on_x

    type (airfoil_type), intent(in)    :: foil
    double precision, intent(in)       :: x_pos
    double precision, intent(in)       :: min_thickness
    double precision, intent(in)       :: scale
    double precision                   :: penalty

    double precision :: y_top, y_bot, thickness_at_x

    penalty = 0d0

    if (x_pos == NOT_DEF_D .or. min_thickness == NOT_DEF_D) return

    y_top = eval_y_on_x (foil%top, x_pos)
    y_bot = eval_y_on_x (foil%bot, x_pos)
    thickness_at_x = y_top - y_bot

    if (thickness_at_x < min_thickness) then
      penalty = (min_thickness - thickness_at_x) * 500d0 ! roughly about 2% less should be 1.0
      penalty = min (penalty, 2d0)    ! cap to avoid extreme values before scaling
      penalty = penalty ** 0.3        ! smaller values more severe
      penalty = penalty * scale
    end if

    call add_to_penalty_stats (PEN_MIN_THICKNESS_AT_X, penalty, thickness_at_x)

  end function penalty_min_thickness_at_x


  function penalty_min_te_top_angle (foil, min_angle, scale) result (penalty)

    !! Penalize a top-side TE tangent angle below the minimum allowed angle.
    !! Top angle is defined as positive when top tangent points downward toward TE.

    type (airfoil_type), intent(in) :: foil
    double precision, intent(in)    :: min_angle
    double precision, intent(in)    :: scale
    double precision                :: penalty

    double precision :: angle_top

    penalty = 0d0

    if (min_angle == NOT_DEF_D) return

    ! estimation of penalty for te angle below min:
    ! if min angle is 10 degrees then max penalty should be for angle of 11 degrees, 
    ! this should be penalty = 1.0 - not scaled yet, 
    ! so penalty = (min_angle - angle) / (11 - 10) = (min_angle - angle) / 1

    angle_top = te_angle_top (foil)
    if (angle_top < min_angle) then
      penalty = (min_angle - angle_top) * 1.0d0
      penalty = min (penalty, 2d0)
      penalty = penalty ** 0.5
      penalty = penalty * scale
    end if

    call add_to_penalty_stats (PEN_MIN_TE_TOP_ANGLE, penalty, angle_top)

  end function penalty_min_te_top_angle


  function penalty_max_te_bot_angle (foil, max_angle, scale) result (penalty)

    !! Penalize a bottom-side TE tangent angle above the maximum allowed angle.
    !! Bottom angle is defined positive when tangent points downward toward TE.
    !! Therefore max_te_bot_angle = 0 enforces horizontal or rising toward TE.

    type (airfoil_type), intent(in) :: foil
    double precision, intent(in)    :: max_angle
    double precision, intent(in)    :: scale
    double precision                :: penalty

    double precision :: angle_bot

    penalty = 0d0

    if (max_angle == NOT_DEF_D) return

    angle_bot = te_angle_bot (foil)
    if (angle_bot > max_angle) then
      penalty = (angle_bot - max_angle) * 1.0d0
      penalty = min (penalty, 2d0)
      penalty = penalty ** 0.5
      penalty = penalty * scale
    end if

    call add_to_penalty_stats (PEN_MAX_TE_BOT_ANGLE, penalty, angle_bot)

  end function penalty_max_te_bot_angle


  function penalty_curv_of_side (side, curv_constraints) result (penalty)

    !! evaluate curvature penalty for side of airfoil 
    !   as sum of penalties for reversals, bumps. le curvature, te curvature etc.

    use airfoil_base,     only : is_bot

    type (side_airfoil_type), intent(in)  :: side 
    type (curv_side_constraints_type), intent(in)  :: curv_constraints
    double precision                     :: penalty

    double precision, allocatable         :: x (:), curv(:)
    double precision                      :: p1, p2, p3, p4
    type (curv_side_constraints_type)     :: c_spec


    ! sanity checks - curvature data should be available for this side
    if (.not. allocated(side%curvature)) then 
      call my_stop("penalty_curv: curvature data not available for "//side%name//" side")
    end if

    x      = side%x 
    curv   = side%curvature
    c_spec = curv_constraints

    p1 = 0d0
    p2 = 0d0
    p3 = 0d0
    p4 = 0d0

    ! LE curvature should be monotonically decreasing from le to te

    if (c_spec%check_le_curvature) then 

      p1 = penalty_le_curv_monoton (x, curv, scale=0.01d0)

    end if

    ! TE curvature, should be below max_te_curvature
    
    p2 = penalty_te_curv    (curv, c_spec%max_te_curvature, c_spec%max_curv_reverse, threshold=0.05d0, &
                             scale=0.01d0)

    ! curvature reversals, should be below max_reversals

    p3 = penalty_reversals  (x, curv, x_start=0.1d0, x_end=1.0d0, max_reversals=c_spec%max_curv_reverse, &
                             threshold=c_spec%curv_threshold, scale=0.005d0)

    ! curvature derivative (bumps), check for sign changes in derivatives

    if (c_spec%check_curvature_bumps) then 
      
      p4 = penalty_bumpiness (x, curv, max_reversals=c_spec%max_curv_reverse)
            
    end if

    penalty = p1 + p2 + p3 + p4

    ! print *, "Curvature penalty components for "//side%name//" side: le_curv_monoton: ", strf('f8.6', p1), &
    !          " te_curv: ", strf('f8.6', p2), " reversals: ", strf('f8.6', p3), " bumps: ", strf('f8.6', p4)

  end function penalty_curv_of_side


  function penalty_curv (foil, curv_constraints) result (penalty)

    !! evaluate absolute curvature violation for foil as sum of penalties for
    !! reversals, bumps. le curvature, te curvature etc.

    type (airfoil_type), intent(in)           :: foil
    type (curv_constraints_type), intent(in)  :: curv_constraints
    double precision                          :: penalty

    double precision     :: p_top, p_bot

    penalty = 0d0

    ! checks active?
    
    if (.not. curv_constraints%check_curvature) return

    ! evaluate penalty for top and bot side and sum up

    p_top = penalty_curv_of_side (foil%top, curv_constraints%top)
    if (.not. foil%symmetrical) then 
      p_bot = penalty_curv_of_side (foil%bot, curv_constraints%bot)
    else 
      p_bot = 0d0
    end if 

    ! scale up - typically is 0.000xxx - for objective function

    penalty = (p_top + p_bot) * PEN_CURVE_SCALE
    penalty = min (penalty, 1d0)   ! cap to avoid extreme values
    penalty = round (penalty, PENALTY_DECIMALS)

  end function penalty_curv


  !--  private  ------------------------------------------------------------------------------

  subroutine add_to_penalty_stats (pen_id, penalty, trigger_value) 

    !! update penalty statistics with new penalty 

    integer, intent(in)          :: pen_id
    double precision, intent(in) :: penalty
    double precision, intent(in), optional :: trigger_value  ! measured value associated with this penalty

    type(penalty_stat_type) :: s
    double precision :: delta

    if (.not. allocated (penalty_stats)) call penalty_stats_init ()

    !$omp critical

    if (pen_id >= 1 .and. pen_id <= N_PENALTY_TYPES) then
      s      = penalty_stats(pen_id)
      s%last = penalty   ! always record the last value, even if zero
      s%trigger_value = NOT_DEF_D
      if (present(trigger_value)) s%trigger_value = trigger_value

      if (penalty > 0d0) then
        delta     = penalty - s%average   ! deviation before count update
        s%count   = s%count + 1
        s%total   = s%total + penalty
        s%average = s%average + delta / s%count
        s%max     = max(s%max, penalty)
      end if
      
      penalty_stats(pen_id) = s
    end if

    !$omp end critical

  end subroutine


  subroutine print_penalty_stats_table (indent)

    !! print current penalty statistics as a table

    integer, intent(in), optional :: indent
    integer :: i, ind
    character(:), allocatable  :: pad
    character(120)  :: row

    if (present (indent)) then
      ind = indent
    else
      ind = 10
    end if

    if (.not. allocated (penalty_stats)) return
    if (all (penalty_stats%count == 0)) return

    pad = repeat(' ', ind)

    call print_colored (COLOR_PALE, pad//"Penalty statistics:")
    print *
    write (row, '(a12, a7, 5a10)') "Name","Count", "Last", "Average", "Max"
    call print_colored (COLOR_PALE, pad//trim(row))
    print *

    do i = 1, N_PENALTY_TYPES
      if (penalty_stats(i)%count > 0) then
        associate (s => penalty_stats(i))
          write (row, '(a12, i7, 5f10.6)') s%name, s%count, s%last, s%average, s%max
          call print_colored (COLOR_NOTE, pad//trim(row))
          print *
        end associate
      end if
    end do

  end subroutine


  function has_penalty (penalty_id) result (has)

    !! check if a penalty type has been recorded in the stats

    integer, intent(in) :: penalty_id
    logical             :: has

    if (.not. allocated (penalty_stats)) then
      has = .false.
    else if (penalty_id >= 1 .and. penalty_id <= N_PENALTY_TYPES) then
      has = (penalty_stats(penalty_id)%count > 0)
    else
      has = .false.
    end if

  end function has_penalty



  subroutine print_penalty_stats (indent)

    !! print penalty statistics in a compact one-line form

    integer, intent(in), optional :: indent
    integer :: i, ind
    logical :: first
    character(:), allocatable :: pad

    if (.not. allocated (penalty_stats)) return
    if (all (penalty_stats%count == 0)) return

    if (present (indent)) then
      ind = indent
    else
      ind = 10
    end if
    pad = repeat(' ', ind)

    call print_colored (COLOR_PALE, pad//"Penalty statistics: ")

    first = .true.
    do i = 1, N_PENALTY_TYPES
      if (penalty_stats(i)%count > 0) then
        if (first) then
          first = .false.
        else
          call print_colored (COLOR_PALE, ", ")
        end if
        call print_colored (COLOR_NOTE, stri(penalty_stats(i)%count)//" "//penalty_stats(i)%name)
      end if
    end do
    print *

  end subroutine


  subroutine print_penalty_stats_avg (indent)

    !! print penalty statistics average in a compact one-line form

    integer, intent(in), optional :: indent
    integer :: i, ind
    logical :: first
    character(:), allocatable :: pad

    if (.not. allocated (penalty_stats)) return
    if (all (penalty_stats%count == 0)) return

    if (present (indent)) then
      ind = indent
    else
      ind = 10
    end if
    pad = repeat(' ', ind)
    call print_colored (COLOR_PALE, pad)

    first = .true.
    do i = 1, N_PENALTY_TYPES
      if (penalty_stats(i)%count > 0) then
        if (first) then
          first = .false.
        else
          call print_colored (COLOR_PALE, ", ")
        end if
        call print_colored (COLOR_NOTE, penalty_stats(i)%name//":"//strf('F9.6', penalty_stats(i)%average,.true.))
      end if
    end do
    print *

  end subroutine


  subroutine print_penalty_stats_trigger (indent)

    !! print penalty statistics trigger value in a compact one-line form

    integer, intent(in), optional :: indent
    integer :: i, ind
    logical :: first
    character(:), allocatable :: pad

    if (.not. allocated (penalty_stats)) return
    if (all (penalty_stats%count == 0)) return

    if (present (indent)) then
      ind = indent
    else
      ind = 10
    end if
    pad = repeat(' ', ind)
    call print_colored (COLOR_PALE, pad)

    first = .true.
    do i = 1, N_PENALTY_TYPES
      if (penalty_stats(i)%count > 0) then
        if (first) then
          first = .false.
        else
          call print_colored (COLOR_PALE, ", ")
        end if
        if (penalty_stats(i)%trigger_value /= NOT_DEF_D) then
           call print_colored (COLOR_NOTE, penalty_stats(i)%name//" at "//strf('F7.4', penalty_stats(i)%trigger_value))
        else
           call print_colored (COLOR_NOTE, penalty_stats(i)%name)
        end if
      end if
    end do
    print *

  end subroutine



  subroutine print_penalty_stats_last (indent)

    !! print last penalty in a compact one-line form

    integer, intent(in), optional :: indent
    integer :: i
    logical :: first
    character(:), allocatable :: pad

    if (.not. allocated (penalty_stats)) return
    if (all (penalty_stats%count == 0)) return

    if (present (indent)) then
      pad = repeat(' ', indent)
    else
      pad = repeat(' ', 10)
    end if
    call print_colored (COLOR_PALE, pad)

    first = .true.
    do i = 1, N_PENALTY_TYPES
      if (has_penalty(i) .and. penalty_stats(i)%last > 0d0) then
        if (first) then
          first = .false.
        else
          call print_colored (COLOR_PALE, ", ")
        end if
        call print_colored (COLOR_NOTE, penalty_stats(i)%name//":"//strf('F9.6', penalty_stats(i)%last,.true.))
      end if
    end do
    print *

  end subroutine


  subroutine penalty_stats_init ()

    !! initialize (or reset) penalty statistics including names

    !$omp critical
    penalty_stats = [                                                               &
      penalty_stat_type("min_thickness"    , 0, 0d0, NOT_DEF_D, 0d0, 0d0, 0d0, 0d0, 0d0), &
      penalty_stat_type("max_thickness"    , 0, 0d0, NOT_DEF_D, 0d0, 0d0, 0d0, 0d0, 0d0), &
      penalty_stat_type("min_camber"       , 0, 0d0, NOT_DEF_D, 0d0, 0d0, 0d0, 0d0, 0d0), &
      penalty_stat_type("max_camber"       , 0, 0d0, NOT_DEF_D, 0d0, 0d0, 0d0, 0d0, 0d0), &
      penalty_stat_type("min_te_angle"     , 0, 0d0, NOT_DEF_D, 0d0, 0d0, 0d0, 0d0, 0d0), &
      penalty_stat_type("min_te_top_angle" , 0, 0d0, NOT_DEF_D, 0d0, 0d0, 0d0, 0d0, 0d0), &
      penalty_stat_type("max_te_bot_angle" , 0, 0d0, NOT_DEF_D, 0d0, 0d0, 0d0, 0d0, 0d0), &
      penalty_stat_type("min_thickness_at_x", 0, 0d0, NOT_DEF_D, 0d0, 0d0, 0d0, 0d0, 0d0), &
      penalty_stat_type("le_curv"          , 0, 0d0, NOT_DEF_D, 0d0, 0d0, 0d0, 0d0, 0d0), &
      penalty_stat_type("le_monoton"       , 0, 0d0, NOT_DEF_D, 0d0, 0d0, 0d0, 0d0, 0d0), &
      penalty_stat_type("te_curv"          , 0, 0d0, NOT_DEF_D, 0d0, 0d0, 0d0, 0d0, 0d0), &
      penalty_stat_type("reversals"        , 0, 0d0, NOT_DEF_D, 0d0, 0d0, 0d0, 0d0, 0d0), &
      penalty_stat_type("bumps"            , 0, 0d0, NOT_DEF_D, 0d0, 0d0, 0d0, 0d0, 0d0), &
      penalty_stat_type("d4_bspline"       , 0, 0d0, NOT_DEF_D, 0d0, 0d0, 0d0, 0d0, 0d0)]
    !$omp end critical

  end subroutine



  function penalty_reversals (x, curv, x_start, x_end, max_reversals, threshold, scale, no_stats) result (penalty)

    !!  Penalize curvature sign changes beyond the allowed reversal count.
    !!  Penalizes the least significant segments rather than all values after a threshold.
    !!  Mirrors improved Python _penalty_reversals logic.

    double precision, intent(in) :: x(:), curv(:)
    double precision, intent(in) :: x_start, x_end, scale
    integer, intent(in)          :: max_reversals
    double precision, intent(in) :: threshold
    logical, intent(in), optional :: no_stats
    double precision             :: penalty

    integer               :: i, n_changes, n_segments_to_keep, n_segments
    integer               :: start_idx, end_idx
    integer, allocatable  :: signs(:), sign_change_idx(:), segment_bounds(:)
    double precision, allocatable :: curv_body(:), segment_maxs(:)
    double precision      :: max_val, penalty_sum
    logical               :: record_stats

    penalty = 0d0

    if (present(no_stats)) then
      record_stats = .not. no_stats
    else
      record_stats = .true.
    end if 

    ! extract region  (pack = numpy array[mask])
    curv_body = pack (curv, x >= x_start .and. x <= x_end)

    ! take only curvature values above threshold into account for sign change counting  
    ! use 0.99 to be save with count_reversals()
    if (threshold > 0d0) then
      curv_body = pack (curv_body, abs(curv_body) > threshold * 0.99d0) 
    end if

    if (size(curv_body) < 2) then
      if (record_stats) call add_to_penalty_stats (PEN_CURV_REVERSALS, penalty)
      return
    end if

    ! Track sign changes from left to right
    ! signs array: +1 or -1, treating zero as positive to avoid ambiguity
    signs = merge (1, -1, curv_body >= 0d0)

    ! count total sign changes
    n_changes = count (signs(2:) /= signs(:size(signs)-1))
    
    if (n_changes <= max_reversals) then
      if (record_stats) call add_to_penalty_stats (PEN_CURV_REVERSALS, penalty)
      return
    end if

    ! Too many changes - penalize least significant segments
    ! Build array of sign change indices (where np.diff(signs) != 0)
    allocate(sign_change_idx(n_changes))
    n_changes = 0
    do i = 2, size(signs)
      if (signs(i) /= signs(i-1)) then
        n_changes = n_changes + 1
        sign_change_idx(n_changes) = i
      end if
    end do

    ! Split curvature into segments based on sign changes
    ! segment_bounds = [1] + sign_change_idx + [size+1]
    n_segments = n_changes + 1
    allocate(segment_bounds(n_segments + 1))
    segment_bounds(1) = 1
    segment_bounds(2:n_segments) = sign_change_idx(:)
    segment_bounds(n_segments + 1) = size(curv_body) + 1

    ! Calculate max absolute value for each segment
    allocate(segment_maxs(n_segments))
    do i = 1, n_segments
      start_idx = segment_bounds(i)
      end_idx = segment_bounds(i + 1) - 1
      max_val = maxval(abs(curv_body(start_idx:end_idx)))
      segment_maxs(i) = max_val
    end do

    ! Sort segments by max value (descending)
    call sort_vector_descend(segment_maxs)

    ! Keep (max_reversals + 1) most significant segments
    ! Sum the max values minus threshold of the remaining segments to penalize
    n_segments_to_keep = max_reversals + 1
    penalty_sum = 0d0
    do i = n_segments_to_keep + 1, n_segments
      penalty_sum = penalty_sum + max (segment_maxs(i) - threshold, 0d0)
    end do

    ! Apply damping for very high penalties to avoid instability
    if (penalty_sum > 1d0) then
      penalty_sum = penalty_sum ** 0.3d0
    end if

    ! Apply scaling
    penalty = penalty_sum * scale

    if (record_stats) call add_to_penalty_stats (PEN_CURV_REVERSALS, penalty)
    
  end function penalty_reversals



  function penalty_te_curv (curv, max_curv_te, max_reversals, threshold, scale) result (penalty)

    !!  Penalize trailing-edge curvature outside the allowed range [0, max_curv_te].
    !!  A tolerance 'threshold' is applied around the allowed range before penalizing.

    double precision, intent(in) :: curv (:)       ! actual curvature
    double precision, intent(in) :: max_curv_te    ! max allowed TE curvature
    integer, intent(in)          :: max_reversals  ! max allowed curvature reversals
    double precision, intent(in) :: threshold      ! tolerance band (e.g. 0.01)
    double precision, intent(in) :: scale          ! output scale factor
    double precision             :: penalty, max_curv

    double precision :: curv_te, min_allowed, max_allowed

    penalty     = 0d0

    ! sign of max_curv_te depends on whether there are reversals or not

    max_curv = abs(max_curv_te) * (-1d0) ** max_reversals

    min_allowed = min(0d0, max_curv)
    max_allowed = max(0d0, max_curv)

    curv_te     = curv(size(curv))  

    if (curv_te < (min_allowed - threshold)) then
      penalty = abs(curv_te - min_allowed) - threshold
    else if (curv_te > (max_allowed + threshold)) then
      penalty = abs(curv_te - max_allowed) - threshold
    end if

    penalty = penalty * scale             ! scale factor to adjust penalty magnitude

    ! if (penalty > 0d0) then
    !   print *, "TE curvature:", curv_te, "allowed range: [", min_allowed, ", ", max_allowed, "] penalty:", penalty
    ! end if

    call add_to_penalty_stats (PEN_TE_CURV, penalty)

  end function penalty_te_curv


  function penalty_le_curv (curv, target_curv_le, threshold, scale) result (penalty)

    !!  Penalize deviation from the target leading-edge curvature.
    !!   No penalty within 'threshold' of the target. 
    !!   A deviation of 100 is about 1.0 for scale=1

    double precision, intent(in) :: curv (:)       ! actual curvature
    double precision, intent(in) :: target_curv_le ! target LE curvature
    double precision, intent(in) :: threshold      ! tolerance (e.g. 0.01)
    double precision, intent(in) :: scale          ! output scale factor
    double precision             :: penalty

    double precision :: delta, curv_le
    
    curv_le = curv(1)

    delta = abs(abs(curv_le) - abs(target_curv_le))
    if (delta <= threshold) then
      penalty = 0d0
    else
      penalty = (delta - threshold) ** 0.5d0 * 0.1d0 * scale
    end if

    call add_to_penalty_stats (PEN_LE_CURV, penalty)

  end function penalty_le_curv



  function penalty_le_curv_monoton (x, curv, scale) result (penalty)

    !!  Penalize non-monotonous curvature at the leading edge.
    !!  Checks if curvature in the first 10 panels is monotonously descending.
    !!  A penalty is applied if curvature value is higher than the previous one.

    double precision, intent(in) :: x(:)           ! x coordinates
    double precision, intent(in) :: curv (:)       ! actual curvature
    double precision, intent(in) :: scale          ! output scale factor
    double precision             :: penalty, diff
    double precision, allocatable :: curv_le(:)

    integer :: i

    penalty = 0d0

    curv_le = pack (curv, (x >= 0d0) .and. (x <= 0.1d0))

    do i = 1, size(curv_le)-1
      diff = abs(curv_le(i+1)) - abs(curv_le(i)) * 0.99d0
      if (diff > 0d0) then
        penalty = (diff / abs(curv(i))) !** 0.5d0             ! relative increase scaled by current curvature
        ! print *, "LE curvature non-monotonic at panel ", i, ": curv(i)=", curv_le(i), " diff=", diff, " penalty: ", penalty
      end if
    end do

    penalty = penalty * scale

    call add_to_penalty_stats (PEN_LE_CURV_MONOTON, penalty)

  end function penalty_le_curv_monoton



  function penalty_bumpiness (x, curv, max_reversals) result (penalty)

    !!  Penalize non-smooth curvature in the body region (x=0.2..1.0).
    !!
    !!  Measures reversals of the first and second derivative of curvature w.r.t. x.
    !!  Works directly on the sampled curvature, so it is curve-type agnostic
    !!  (Bezier and B-Spline alike).
    !!
    !!  Args:
    !!    x: x coordinates
    !!    curv: Curvature values sampled at x (correct sign already applied)
    !!    max_reversals: Allowed curvature reversals (passed through to curv_dd check)
    !!    scale: Scaling factor for the returned penalty

    use math_util, only: derivative1

    double precision, intent(in) :: x(:), curv(:)
    integer, intent(in)          :: max_reversals
    double precision             :: penalty

    double precision, allocatable :: curv_d(:), curv_dd(:)
    double precision, allocatable :: x_body(:), curv_body(:)
    logical, allocatable          :: body_mask(:)
    double precision              :: x_start, x_end, pen_d, pen_dd

    penalty = 0d0

    ! Sanity check input sizes
    if (size(x) /= size(curv) .or. size(x) < 2) then
      call add_to_penalty_stats (PEN_CURV_BUMPS, penalty)
      return
    end if

    ! Extract body region [0.2, 1.0]
    x_start = 0.1d0
    x_end   = 1.0d0
    body_mask = (x >= x_start) .and. (x <= x_end)    ! exclude TE point to avoid noise from TE curvature behavior

    x_body    = pack (x, body_mask)
    curv_body = pack (curv, body_mask)

    ! Compute first and second derivatives of curvature w.r.t. x
    curv_d  = derivative1(x_body, curv_body)
    curv_dd = derivative1(x_body, curv_d)

    ! Normalize to be similar to curvature values and avoid scale issues with penalty threshold
    curv_d  = curv_d  / 10.0d0
    curv_dd = curv_dd / 100.0d0

    ! Curvature should monotonically decrease in body region
    !  - also becoming negative (reversal of curvature)
    ! -> avoid reversals of derivative of curvature

    pen_d = penalty_reversals (x_body, curv_d, 0d0, 1d0, &
                               max_reversals=0, threshold=0.02d0, scale=0.1d0, no_stats=.true.) 

    ! Derivative of curvature decreases and may increase again
    ! -> allow max_reversals of derivative of derivative

    pen_dd = penalty_reversals (x_body, curv_dd, 0d0, 1d0, &
                                max_reversals=max_reversals, threshold=0.02d0, scale=1.0d0, no_stats=.true.)  

    penalty = (pen_d + pen_dd) / 1000d0

    call add_to_penalty_stats (PEN_CURV_BUMPS, penalty)

  end function penalty_bumpiness



  function penalty_d4_bspline (bspline) result (penalty)

    !!  D4 smoothness penalty for degree-4 uniform B-splines.
    !!
    !!  The 4th derivative of a quartic B-spline is piecewise constant per span
    !!  and proportional to Δ⁴P (4th finite difference of control points).
    !!  Large Δ⁴P values indicate local non-smoothness ("bumps") in the curvature.
    !!
    !!  pen = mean(Δ⁴px²)*fac*0.01 + mean(Δ⁴py²)*fac*1.0,  fac = f(ncp)

    use shape_bspline,  only : bspline_spec_type, BSPLINE_DEGREE

    type(bspline_spec_type), intent(in) :: bspline
    double precision                    :: penalty

    double precision, allocatable :: d4x(:), d4y(:)
    double precision              :: pen_x, pen_y, fac
    integer                       :: ncp
    double precision, parameter   :: fac_table(5) = [2d0, 2d0, 15d0, 25d0, 60d0]

    penalty = 0d0

    ncp    = size(bspline%py)
    if (BSPLINE_DEGREE /= 4 .or. ncp < 6) return

    ! 4th finite differences of control points
    d4x = diff_1D(bspline%px, 4)
    d4y = diff_1D(bspline%py, 4)

    if (size(d4x) == 0 .or. size(d4y) == 0) return

    ! ncp-based empirical scaling factor (mirrors Python fac_table)
    fac = fac_table(min(ncp - 6, 4) + 1)

    pen_x = sum(d4x**2) / size(d4x) * fac * 0.01d0  ! x: curvature evolution
    pen_y = sum(d4y**2) / size(d4y) * fac * 1.0d0   ! y: main bump indicator

    penalty = (pen_x + pen_y) / 100d0

    call add_to_penalty_stats (PEN_D4, penalty)

  end function penalty_d4_bspline


end module 