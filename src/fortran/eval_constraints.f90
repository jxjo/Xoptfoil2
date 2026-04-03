! MIT License
! Copyright (c) 2025 Jochen Guenzel 

module eval_constraints

  !-------------------------------------------------------------------------
  ! evaluations of airfoil geometry constraints
  !-------------------------------------------------------------------------
   
  use os_util
  use string_util,            only : stri, strf
  use airfoil_base,           only: airfoil_type, side_airfoil_type
  use airfoil_geometry,       only: max_curvature_at_te
  use eval_commons

  implicit none
  private 

  public :: geo_constraints_type
  public :: curv_constraints_type
  public :: curv_side_constraints_type

  public :: penalty_curv, penalty_geo, penalty_curv_of_side
  
  public :: assess_surface
  public :: assess_side
  public :: penalty_stats_init, penalty_stats_print, penalty_stats_print_table
  public :: penalty_reversals, penalty_le_curv_monoton
  public :: penalty_te_curv, penalty_le_curv
  public :: penalty_curv_deriv, penalty_curv_deriv_region

  public :: has_penalty, PEN_MIN_TE_ANGLE


  ! penalty ids

  integer, parameter :: PEN_MIN_TE_ANGLE    = 1
  integer, parameter :: PEN_LE_CURV         = 2
  integer, parameter :: PEN_LE_CURV_MONOTON = 3
  integer, parameter :: PEN_TE_CURV         = 4
  integer, parameter :: PEN_CURV_REVERSALS  = 5
  integer, parameter :: PEN_CURV_DERIV      = 6
  integer, parameter :: N_PENALTY_TYPES     = 6


  ! --- private, static ---------------------------------------------------

  type penalty_stat_type
    character (15)   :: name
    integer          :: count                  ! number of violations of this type
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

    !! evaluate geometry constraint violation penalty for foil 
    !! as sum of penalties for thickness, camber, te angle etc.

    use airfoil_geometry,       only : eval_thickness_camber_lines, get_geometry, te_angle

    type (airfoil_type), intent(in)         :: foil
    type (geo_constraints_type), intent(in) :: geometry_constraints
    double precision                        :: penalty

    double precision :: min_angle, angle

    penalty = 0d0

    ! check geometry constraints if active

    if (.not. geometry_constraints%check_geometry) return

    ! Too small TE angle 

    min_angle = geometry_constraints%min_te_angle
    angle = te_angle (foil)
    if (angle < min_angle) then 
      penalty = penalty + (min_angle - angle) * 10d0
      call add_to_penalty_stats (PEN_MIN_TE_ANGLE, penalty)
    end if

  end function penalty_geo


  function penalty_curv_of_side (side, curv_constraints) result (penalty)

    !! evaluate curvature penalty for side of airfoil 
    !   as sum of penalties for reversals, bumps. le curvature, te curvature etc.

    type (side_airfoil_type), intent(in)  :: side 
    type (curv_side_constraints_type), intent(in)  :: curv_constraints
    double precision                     :: penalty

    double precision, allocatable         :: x (:), curv(:)
    double precision          :: p1, p2, p3, threshold, max_te_curv
    integer                   :: max_reversals

    ! sanity checks - curvature data should be available for this side
    if (.not. allocated(side%curvature)) then 
      call my_stop("penalty_curv: curvature data not available for "//side%name//" side")
    end if

    x    = side%x
    curv = side%curvature

    ! TE curvature, should be below max_te_curvature
    
    max_te_curv = curv_constraints%max_te_curvature
    p1 = penalty_te_curv    (curv, max_te_curv, threshold=0.0d0, scale=1.0d0)

    ! curvature reversals, should be below max_reversals

    max_reversals = curv_constraints%max_curv_reverse
    p2 = penalty_reversals  (x, curv, x_start=0.2d0, x_end=1.0d0, max_reversals=max_reversals, scale=1.0d0)

    ! curvature derivative reversals (bumps), should be below max_spikes

    if (curv_constraints%check_curvature_bumps) then 
      threshold = curv_constraints%spike_threshold
      p3 = penalty_curv_deriv (x, curv, threshold_body=threshold, threshold_te=threshold*2d0)
    else 
      p3 = 0d0
    end if

    penalty = p1 + p2 + p3

  end function penalty_curv_of_side


  function penalty_curv (foil, curv_constraints) result (penalty)

    !! evaluate curvature penalty for foil as sum of penalties for reversals, bumps. le curvature, te curvature etc.

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

    penalty = p_top + p_bot

  end function penalty_curv


  subroutine assess_side (side, x_start, x_end, curv_constraints, quality)

    !! Assess curvature of a side of the airfoil and print info about it.
    !! Checks curvature in x range [x_start, x_end] for reversals and bumps and prints a quality string.
    !! Quality is returned as an integer e.g. Q_GOOD, Q_BAD etc.

    use math_util,            only: derivative1, count_reversals
    use print_util,           only: print_text

    type (side_airfoil_type), intent(in)  :: side

    double precision, intent(in) :: x_start, x_end
    type (curv_side_constraints_type), intent(in) :: curv_constraints
    integer, intent(out) :: quality

    double precision, allocatable :: x(:), curv(:), deriv(:)
    integer                       :: n, n_reversals, n_bumps, delta_reversals, delta_bumps
    double precision              :: te_curv, delta_te_curv

    quality = Q_GOOD

    ! extract region (x_start to x_end)
    x    = pack (side%x, side%x >= x_start .and. side%x <= x_end)

    n = size(x)
    if (n < 2) return

    curv  = pack (side%curvature, side%x >= x_start .and. side%x <= x_end)
    deriv = derivative1 (x, curv)

    ! get counts in this region
    n_reversals = count_reversals (1, n, curv,  curv_constraints%curv_threshold)
    n_bumps     = count_reversals (1, n, deriv, curv_constraints%spike_threshold)
    te_curv     = max_curvature_at_te (curv)

    ! assess quality and print 
    delta_reversals = n_reversals - curv_constraints%max_curv_reverse
    delta_bumps     = n_bumps     - curv_constraints%max_spikes
    delta_te_curv   = te_curv     - curv_constraints%max_te_curvature
    print *, side%name//" te_curvature: "//strf('(F5.2)', te_curv)//&
             " ("//strf('(F5.2)', curv_constraints%max_te_curvature)//")"

    call print_text ("Curvature: ",10, no_crlf = .true.)

    if (delta_reversals > 0) then
      call print_colored (COLOR_BAD, stri(n_reversals))
      quality = Q_BAD
    else
      call print_colored (COLOR_GOOD, stri(n_reversals))
    end if
    call print_colored (COLOR_NOTE, " Reversals, ")

    if (delta_bumps > 0) then
      call print_colored (COLOR_BAD, stri(n_bumps))
      quality = Q_BAD
    else
      call print_colored (COLOR_GOOD, stri(n_bumps))
    end if
    call print_colored (COLOR_NOTE, " Bumps, ")

    if (delta_te_curv > 0) then
      call print_colored (COLOR_BAD, strf('(F5.2)',te_curv))
      quality = Q_BAD
    else
      call print_colored (COLOR_GOOD, strf('(F5.2)',te_curv))
    end if
    call print_colored (COLOR_NOTE, " TE curvature")
    print *

  end subroutine assess_side



  subroutine assess_surface (side, show_details, &
                            is, ie, iend_spikes, &
                            curv_threshold, spike_threshold, overall_quality)

    !! Assess polyline (x,y) on surface quality (curvature and derivative of curvature)
    !! - will return surface quality e.g. Q_GOOD
    !! - print an info string like this '-----R---H--sss--' (show_details)

    use math_util,        only : find_reversals, derivative1

    type (side_airfoil_type), intent(in)  :: side 
    logical, intent(in)                   :: show_details
    integer, intent(in)                   :: is, ie, iend_spikes
    double precision, intent(in)          :: curv_threshold, spike_threshold
    integer, intent(out)                  :: overall_quality

    integer             :: nspikes, nreversals, npt
    double precision    :: cur_te_curvature
    integer             :: quality_spikes, quality_reversals
    integer             :: quality_te
    character (size(side%x))     :: result_info
    character (100)           :: result_out
    character(:), allocatable :: info

    info = side%name // " side"

    nreversals = 0
    nspikes    = 0
    npt        = size(side%x)

    result_info           = repeat ('-', npt ) 

    ! have a look at derivative of curvature ... 

    call find_reversals (is, iend_spikes, spike_threshold, derivative1 (side%x, side%curvature), &
                        's', nspikes, result_info)


    ! have a look at curvature 

    call find_reversals(is, ie, curv_threshold, side%curvature, 'R', &
                        nreversals, result_info)
            
    quality_spikes    = i_quality (nspikes, 2, 6, 40)
    quality_reversals = i_quality (nreversals, 2, 3, 10)
    overall_quality   = ior(quality_spikes, quality_reversals)

    ! check te curvature 
    
    cur_te_curvature = max_curvature_at_te (side%curvature )

    ! in case of a reversal, allow a little more curvature e.g. reflexed airfoil
    if (nreversals == 1) then 
      quality_te      = r_quality (cur_te_curvature, 0.4d0, 2d0, 10d0)
    else
      quality_te      = r_quality (cur_te_curvature, 0.2d0, 1d0, 10d0)
    end if 
    ! te quality counts only half as too often it is bad ... 
    overall_quality = ior(overall_quality, (quality_te / 2))

  
    ! all the output ...

    if(show_details) then

      if (len(result_info) > len(result_out)) then
        result_out = '... ' // result_info ((len(result_info) - len(result_out) + 1 + 4):)
      else
        result_out = result_info
      end if 

      call print_colored (COLOR_NOTE, repeat(' ',5) //info//' '//result_out)
      print *

      call print_colored (COLOR_NOTE, repeat(' ',18) // 'Spikes')
      call print_colored_i (3, quality_spikes, nspikes)
      call print_colored (COLOR_NOTE, '     Reversals')
      call print_colored_i (3, quality_reversals, nreversals)
      call print_colored (COLOR_NOTE, '     Curvature at TE')
      call print_colored_r (5,'(F5.2)', quality_te, cur_te_curvature) 
      if (quality_te > Q_BAD) then
        call print_colored (COLOR_NOTE, '  (geometric spoiler at TE?)')
      end if 
      print *
    end if
                              
  end subroutine assess_surface
 


  !--------------------------------------------------------------------------------------



  !--  private  ------------------------------------------------------------------------------

  subroutine add_to_penalty_stats (pen_id, penalty_value) 

    !! update penalty statistics with new penalty 

    integer, intent(in)          :: pen_id
    double precision, intent(in) :: penalty_value

    if (penalty_value <= 0d0) return

    if (.not. allocated (penalty_stats)) call penalty_stats_init ()

    !$omp critical

    if (pen_id >= 1 .and. pen_id <= N_PENALTY_TYPES) then
      block
        type(penalty_stat_type) :: s
        double precision :: delta
        s         = penalty_stats(pen_id)
        delta     = penalty_value - s%average   ! deviation before count update
        s%count   = s%count + 1
        s%total   = s%total + penalty_value
        s%average = s%average + delta / s%count
        s%M2      = s%M2 + delta * (penalty_value - s%average)  ! uses updated average
        s%std     = sqrt(s%M2 / s%count)
        s%max     = max(s%max, penalty_value)
        penalty_stats(pen_id) = s
      end block
    end if

    !$omp end critical

  end subroutine


  subroutine penalty_stats_print_table (indent)

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
    write (row, '(a4, a11, a6, 4a10)') "Name", "","Count", "Average", "Std", "CV", "Max"
    call print_colored (COLOR_PALE, pad//trim(row))
    print *

    do i = 1, N_PENALTY_TYPES
      if (penalty_stats(i)%count > 0) then
        associate (s => penalty_stats(i))
          write (row, '(a15, i6, 4f10.3)') s%name, s%count, s%average, s%std, &
                                           merge(s%std / s%average, 0d0, s%average > 0d0), s%max
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



  subroutine penalty_stats_print (indent)

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
        call print_colored (COLOR_NOTE, stri(penalty_stats(i)%count)//" "//trim(penalty_stats(i)%name))
      end if
    end do
    print *

  end subroutine



  subroutine penalty_stats_init ()

    !! initialize (or reset) penalty statistics including names

    !$omp critical
    penalty_stats = [                                                          &
      penalty_stat_type("min_te_angle", 0, 0d0, 0d0, 0d0, 0d0, 0d0),           &
      penalty_stat_type("le_curv"   ,   0, 0d0, 0d0, 0d0, 0d0, 0d0),           &
      penalty_stat_type("le_monoton",   0, 0d0, 0d0, 0d0, 0d0, 0d0),           &
      penalty_stat_type("te_curv"   ,   0, 0d0, 0d0, 0d0, 0d0, 0d0),           &
      penalty_stat_type("reversals" ,   0, 0d0, 0d0, 0d0, 0d0, 0d0),           &
      penalty_stat_type("bumps"     ,   0, 0d0, 0d0, 0d0, 0d0, 0d0) ]
    !$omp end critical

  end subroutine



  function penalty_reversals (x, curv, x_start, x_end, max_reversals, scale) result (penalty)

    !!  Penalize curvature sign changes beyond the allowed reversal count.
    !!  Smooth penalty - grows continuously once sign changes exceed max_reversals.
    !!  Mirrors Python Matcher_Base._penalty_reversals logic.

    double precision, intent(in) :: x(:), curv(:)
    double precision, intent(in) :: x_start, x_end, scale
    integer, intent(in)          :: max_reversals
    double precision             :: penalty

    integer               :: i, n_changes, violation_start, locked_sign
    integer, allocatable  :: signs(:)
    double precision, allocatable :: curv_body(:), violations(:), after(:)
    double precision      :: ref

    penalty = 0d0

    ! extract region  (pack = numpy array[mask])
    curv_body = pack (curv, x >= x_start .and. x <= x_end)
    if (size(curv_body) < 2) return

    ! signs array: +1 or -1  (merge = numpy where)
    signs = merge (1, -1, curv_body >= 0d0)

    ! count total sign changes  (count = numpy sum on bool array)
    n_changes = count (signs(2:) /= signs(:size(signs)-1))
    if (n_changes <= max_reversals) return      ! within allowed - no penalty

    ! find violation start index and locked sign
    if (max_reversals == 0) then
      violation_start = 1
      locked_sign     = 1                       ! positive expected from start
    else
      n_changes = 0
      violation_start = size(signs)             ! fallback
      do i = 2, size(signs)
        if (signs(i) /= signs(i-1)) then
          n_changes = n_changes + 1
          if (n_changes == max_reversals + 1) then
            violation_start = i
            exit
          end if
        end if
      end do
      locked_sign = signs(violation_start)
    end if

    ! violations from violation_start onward  (merge = numpy where)
    after = curv_body(violation_start:)
    if (locked_sign > 0) then
      violations = merge (-after, 0d0, after < 0d0)   ! penalize negative
    else
      violations = merge ( after, 0d0, after > 0d0)   ! penalize positive
    end if
    ! normalize by peak curvature -> violations in [0,1], penalty in [0,1] for scale=1
    ref = maxval (abs(curv_body))
    if (ref < 1d-10) return

    penalty = sum (violations / ref) / size(after) * 30d0 * scale

    call add_to_penalty_stats (PEN_CURV_REVERSALS, penalty)
    
  end function penalty_reversals



  function penalty_te_curv (curv, max_curv_te, threshold, scale) result (penalty)

    !!  Penalize trailing-edge curvature outside the allowed range [0, max_curv_te].
    !!  A tolerance 'threshold' is applied around the allowed range before penalizing.

    double precision, intent(in) :: curv (:)       ! actual curvature
    double precision, intent(in) :: max_curv_te    ! max allowed TE curvature
    double precision, intent(in) :: threshold      ! tolerance band (e.g. 0.01)
    double precision, intent(in) :: scale          ! output scale factor
    double precision             :: penalty

    double precision :: curv_te, min_allowed, max_allowed

    penalty     = 0d0
    min_allowed = min(0d0, max_curv_te)
    max_allowed = max(0d0, max_curv_te)
    curv_te     = max_curvature_at_te (curv)

    if (curv_te < min_allowed - threshold) then
      penalty = (abs(curv_te - min_allowed) - threshold) * scale
    else if (curv_te > max_allowed + threshold) then
      penalty = (abs(curv_te - max_allowed) - threshold) * scale
    end if

    penalty = penalty * 0.2d0 * scale ! scale factor to adjust penalty magnitude

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



  function penalty_le_curv_monoton (curv, scale) result (penalty)

    !!  Penalize non-monotonous curvature at the leading edge.
    !!  Checks if curvature in the first 10 panels is monotonously descending.
    !!  A penalty is applied if curvature value is more than 1% higher than the previous one.

    double precision, intent(in) :: curv (:)       ! actual curvature
    double precision, intent(in) :: scale          ! output scale factor
    double precision             :: penalty

    integer :: i

    penalty = 0d0

    do i = 1, min(10, size(curv)-1)
      if (abs(curv(i)) < abs(curv(i+1)) / 1.01d0) then
        penalty = penalty + (abs(curv(i+1)) - abs(curv(i))) * scale * 0.1d0
      end if
    end do

    call add_to_penalty_stats (PEN_LE_CURV_MONOTON, penalty)

  end function penalty_le_curv_monoton



  function penalty_curv_deriv (x, curv, threshold_body, threshold_te) result (penalty)

    !!  Master penalty function for curvature derivative (surface quality/fairness).
    !!  Evaluates penalty in body region (0.3-0.8) and trailing edge region (0.8-1.0).
    !!  Uses Huber-hinge response to suppress bumps while keeping objective well-behaved.

    use math_util, only: derivative1

    double precision, intent(in) :: x(:), curv(:)
    double precision, intent(in) :: threshold_body  ! threshold for body region
    double precision, intent(in) :: threshold_te    ! threshold for TE region
    double precision             :: penalty

    double precision, allocatable :: curv_deriv(:)

    ! Compute derivative once
    curv_deriv = derivative1(x, curv)

    ! Sum penalties from body and TE regions
    penalty = penalty_curv_deriv_region(x, curv_deriv, 0.4d0, 0.8d0, threshold_body, 1.0d0) + &
              penalty_curv_deriv_region(x, curv_deriv, 0.8d0, 1.0d0, threshold_te,   2.0d0)

  end function penalty_curv_deriv



  function penalty_curv_deriv_region (x, curv_deriv, x_start, x_end, threshold, scale) result (penalty)

    !!  Penalize large curvature derivatives in a given x region.
    !!  This acts as a smoothness term that suppresses bumps while using a
    !!  Huber-hinge style response to keep the objective well behaved near threshold.
    !!  Mirrors Python Matcher_Base._penalty_curv_deriv logic.
    !!  Requires pre-computed derivative for performance.

    double precision, intent(in) :: x(:), curv_deriv(:)
    double precision, intent(in) :: x_start, x_end
    double precision, intent(in) :: threshold  ! allowed curvature-derivative magnitude
    double precision, intent(in) :: scale      ! scaling factor
    double precision             :: penalty

    double precision, allocatable :: deriv_body(:), excess(:), penalty_vals(:)
    double precision              :: huber_delta
    integer                       :: i, n

    penalty = 0d0

    ! extract region (x_start to x_end)
    deriv_body = pack(curv_deriv, x >= x_start .and. x <= x_end)

    n = size(deriv_body)
    if (n < 2) return

    ! hinge: excess = abs(deriv_body) - threshold (zero below threshold)
    allocate(excess(n))
    excess = abs(deriv_body) - threshold

    ! Huber delta: transition from quadratic to linear
    huber_delta = threshold * 0.5d0

    ! vectorized Huber-Hinge:
    !   excess <= 0            → 0                                    (no penalty below threshold)
    !   0 < excess <= delta    → excess²/2                            (quadratic: smooth near threshold)
    !   excess > delta         → delta*(excess - delta/2)             (linear: robust far away)

    allocate(penalty_vals(n))
    do i = 1, n
      if (excess(i) <= 0d0) then
        penalty_vals(i) = 0d0
      else if (excess(i) <= huber_delta) then
        penalty_vals(i) = 0.5d0 * excess(i)**2
      else
        penalty_vals(i) = huber_delta * (excess(i) - 0.5d0 * huber_delta)
      end if
    end do

    penalty = (sum(penalty_vals) / n) * 0.5d0 * scale

    call add_to_penalty_stats (PEN_CURV_DERIV, penalty)

  end function penalty_curv_deriv_region


end module 