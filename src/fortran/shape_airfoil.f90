! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2025 Jochen Guenzel 

module shape_airfoil

  !-------------------------------------------------------------------------
  ! Create an airfoil shape from design variables
  !-------------------------------------------------------------------------

  use os_util
  use string_util,            only : stri
  use airfoil_base,           only : airfoil_type, is_hh_based, build_from_hh_seed
  use commons,                only : TOP, BOT
  use shape_bezier,           only : shape_bezier_type, shape_bezier_ndv
  use shape_bspline,          only : shape_bspline_type, shape_bspline_ndv
  use shape_hicks_henne,      only : shape_hh_type, shape_hh_ndv
  use xfoil_driver,           only : flap_spec_type

  implicit none
  private 

  ! --- shape master type specifying how airfoil will shaped ---------------------------- 

  integer, parameter, public  :: HICKS_HENNE = 1          ! shape types 
  integer, parameter, public  :: BEZIER      = 2
  integer, parameter, public  :: BSPLINE     = 3

  type shape_spec_type
    integer                       :: type                 ! HICKS_HENNE or BEZIER or ...
    character (:), allocatable    :: type_as_text         ! eg 'hicks-henne' 
    character (:), allocatable    :: camber_type          ! either '' or 'reflexed' or 'rear-loading' 
    
    integer                       :: ndv                  ! number of design variables 
    type(shape_hh_type)           :: hh                   ! specs for hicks-henne
    type(shape_bezier_type)       :: bezier               ! specs for bezier 
    type(shape_bspline_type)      :: bspline              ! specs for bspline 

    type (flap_spec_type)         :: flap_spec

  end type

  public          :: shape_spec_type


  ! --- Public functions------------------------------------------------------- 

  public :: assess_shape
  public :: set_shape_spec
  public :: get_seed_foil
  public :: get_ndv_of_shape
  public :: get_ndv_of_flaps
  public :: get_dv0_of_shape
  public :: get_dv0_of_flaps
  public :: get_dv_initial_perturb_of_shape
  public :: get_dv_initial_perturb_of_flaps
  public :: create_airfoil_bezier
  public :: create_airfoil_bspline
  public :: create_airfoil_hicks_henne
  public :: get_flap_angles_optimized
  public :: debug_print_dv_as_shape_data
  public :: debug_print_vel_as_shape_data

  ! --- Public static ------------------------------------------------------- 

  type (shape_spec_type), public    :: shape_spec           ! main shape specification, static 


  ! ---- private, static -------------------------------------------------------
  
  type (airfoil_type)               :: seed_foil


contains

  subroutine assess_shape ()

    !-----------------------------------------------------------------------------
    !! make an assessment of defined shape functions if it's suitable for optimization  
    !-----------------------------------------------------------------------------

    use print_util
    use commons,         only : show_details 

    integer             :: ndv, ndv_top, ndv_bot, ndv_flap
    logical             :: top_reversal, bot_reversal 

    if (.not. show_details) return 

    ! is airfoil reflexed or has rear-loading? Maybe increase design variables... 

    top_reversal = .false.
    bot_reversal = .false.

    if (shape_spec%camber_type == 'reflexed') then 
      call print_note ("1 reversal on Top side - likely the airfoil will be ", 3, no_crlf=.true.)
      call print_colored (COLOR_NORMAL, "reflexed")
      print *
      top_reversal = .true.
    else if (shape_spec%camber_type == 'rear-loading') then 
      call print_note ("1 reversal on Bot side - likely the airfoil will have ", 3, no_crlf=.true.)
      call print_colored (COLOR_NORMAL, 'rear-loading')
      print *
      bot_reversal = .true.
    end if 

    ! Assess design variables  depending on shape function 

    call print_action   ("Using shape functions ", no_crlf=.true.)
    call print_colored  (COLOR_FEATURE, shape_spec%type_as_text)
    call print_colored (COLOR_NOTE, " to create airfoils")
    print *
 
    if (shape_spec%type == BEZIER) then 

      call assess_side_bezier ("Top", shape_spec%bezier%ncp_top, top_reversal, .false., ndv_top)
      call assess_side_bezier ("Bot", shape_spec%bezier%ncp_bot, bot_reversal, .true.,  ndv_bot)
      ndv = ndv_top + ndv_bot 

    else if (shape_spec%type == BSPLINE) then 

      call assess_side_bspline ("Top", shape_spec%bspline%ncp_top, top_reversal, .false., ndv_top)
      call assess_side_bspline ("Bot", shape_spec%bspline%ncp_bot, bot_reversal, .true.,  ndv_bot)
      ndv = ndv_top + ndv_bot 

    else if (shape_spec%type == HICKS_HENNE) then

      call assess_side_hh ("Top", shape_spec%hh%nfunctions_top, top_reversal, ndv_top)
      call assess_side_hh ("Bot", shape_spec%hh%nfunctions_bot, bot_reversal, ndv_bot)
      ndv = ndv_top + ndv_bot 

    else 

      ndv = 0 

    end if 

    ! Flaps to be optimized? 

    ndv_flap = shape_spec%flap_spec%ndv

    if (ndv_flap > 0 ) then 

      ndv = ndv + ndv_flap 
      call print_action   ("There are "//stri(ndv_flap)//" operationg points with ", no_crlf=.true.)
      call print_colored  (COLOR_FEATURE, "flap optimization")
      print *

    end if 

    call print_action   ("A total of "//stri(ndv)//" design variables will be optimized")

  end subroutine



  subroutine assess_side_bezier (side, ncp, has_reversal, c2_coupled, ndv)

    !-----------------------------------------------------------------------------
    !! assess top or bot side of bezier spec  
    !-----------------------------------------------------------------------------

    use print_util
    use shape_bezier,     only : ncp_to_ndv

    character (*), intent(in)     :: side
    logical, intent(in)           :: has_reversal 
    integer, intent(in)           :: ncp
    logical, intent(in)           :: c2_coupled
    integer, intent(out)          :: ndv

    ndv = ncp_to_ndv(ncp, c2_coupled)

    call print_text (side//" side  ", indent=5, no_crlf=.true.) 

    if (ncp > 0) then 
      call print_text (stri(ncp)// " bezier control points - needs "//stri(ndv)//" design variables") 

      if (has_reversal .and. (ncp <= 5)) then 
        call print_note ("Because of curve reversal, consider to increase no of control points",5)
      end if 
    else 
      call print_text (" no bezier control points") 
    end if

  end subroutine


  subroutine assess_side_bspline (side, ncp, has_reversal, c2_coupled, ndv)

    !-----------------------------------------------------------------------------
    !! assess top or bot side of bspline spec  
    !-----------------------------------------------------------------------------

    use print_util
    use shape_bspline,     only : ncp_to_ndv

    character (*), intent(in)     :: side
    logical, intent(in)           :: has_reversal 
    integer, intent(in)           :: ncp
    logical, intent(in)           :: c2_coupled
    integer, intent(out)          :: ndv

    ndv = ncp_to_ndv(ncp, c2_coupled)

    call print_text (side//" side  ", indent=5, no_crlf=.true.) 

    if (ncp > 0) then 
      call print_text (stri(ncp)// " bspline control points - needs "//stri(ndv)//" design variables") 

      if (has_reversal .and. (ncp <= 5)) then 
        call print_note ("Because of curve reversal, consider to increase no of control points",5)
      end if 
    else 
      call print_text (" no bspline control points") 
    end if

  end subroutine


  subroutine assess_side_hh (side, nfunctions, has_reversal, ndv)

    !-----------------------------------------------------------------------------
    !! assess top or bot side of hicks henne spec  
    !-----------------------------------------------------------------------------

    use print_util
    use shape_hicks_henne,     only : nfunctions_to_ndv

    character (*), intent(in)     :: side
    logical, intent(in)           :: has_reversal 
    integer, intent(in)           :: nfunctions
    integer, intent(out)          :: ndv


    ndv = nfunctions_to_ndv (nfunctions)

    call print_text (side//" side  ", indent=5, no_crlf=.true.) 

    if (nfunctions > 0) then 
      call print_text (stri(nfunctions)// " hicks henne functions - needs "//stri(ndv)//" design variables") 

      if (has_reversal .and. (nfunctions <= 3)) then 
        call print_note ("Because of curve reversal, consider to increase no of functions on "//&
                        side//" side",5)
      end if 
    else
      call print_text (" no hicks henne functions") 
    end if  

  end subroutine



  subroutine set_shape_spec (seed_foil_in, shape_spec_in)

    !-----------------------------------------------------------------------------
    !! set seed airfoil and shape_spec into static mod variable for later evaluation  
    !-----------------------------------------------------------------------------

    type (airfoil_type), intent(in)     :: seed_foil_in 
    type (shape_spec_type), intent(in)  :: shape_spec_in 

    seed_foil = seed_foil_in
    shape_spec = shape_spec_in

  end subroutine 



  function get_seed_foil () result (seed_foil_out)

    !! gets seed_foil from static mod variable 

    type (airfoil_type)  :: seed_foil_out 

    seed_foil_out = seed_foil

  end function get_seed_foil



  subroutine create_airfoil_hicks_henne (dv, foil)

    !-----------------------------------------------------------------------------
    !! Creates an airfoil surface by perturbing an input "seed" airfoil
    !! with Hicks-Henne shape functions
    !-----------------------------------------------------------------------------

    use airfoil_base,           only : build_from_sides
    use shape_hicks_henne,      only : nfunctions_to_ndv, hh_spec_type, map_dv_to_hhs, hh_eval_side

    double precision, intent(in)    :: dv(:) 
    type(airfoil_type), intent(out) :: foil 

    double precision, allocatable   :: dv_top (:), dv_bot (:)
    integer                         :: ndv_top

    type (hh_spec_type)             :: top_hh_spec , bot_hh_spec


    ! top side - design variables to hh functions 

    ndv_top = nfunctions_to_ndv (shape_spec%hh%nfunctions_top)
    dv_top = dv (1: ndv_top)
    call map_dv_to_hhs (dv_top, top_hh_spec%hhs)                       ! rebuild hicks henne specs 

    ! bot side - design variables to hh functions 

    if (.not. seed_foil%symmetrical) then

      dv_bot = dv (ndv_top + 1 : )
      call map_dv_to_hhs (dv_bot, bot_hh_spec%hhs)
  
    end if

    ! prepare foil to be build from seed and hh functions 

    if (is_hh_based (seed_foil)) then               ! ... take the original seed foil of seed foil
      foil%hh_seed_x = seed_foil%hh_seed_x
      foil%hh_seed_y = seed_foil%hh_seed_y
      foil%hh_seed_filename  = seed_foil%hh_seed_filename 
    else                                          ! ... normal .dat seed: take its coordinates
      foil%hh_seed_x = seed_foil%x
      foil%hh_seed_y = seed_foil%y
      foil%hh_seed_filename  = seed_foil%filename        
    end if 
    foil%top%hh = top_hh_spec
    foil%bot%hh = bot_hh_spec
    foil%symmetrical   = seed_foil%symmetrical

    ! eval hh and build it 

    call build_from_hh_seed (foil)

  end subroutine 



  subroutine create_airfoil_bezier (dv, foil) 

    !-----------------------------------------------------------------------------
    !! Create airfoil from bezier design variables
    !!  - seed is only needed to determine TE gap and number of points 
    !!  - C2 coupling at LE: py(2) of bot is derived from le_curv so that
    !!    curvature at leading edge is identical for top and bot
    !-----------------------------------------------------------------------------

    use airfoil_base,           only : split_foil_into_sides, airfoil_from_bezier, te_gap
    use shape_bezier,           only : bezier_spec_type, bezier_curvature, bezier_le_curvature
    use shape_bezier,           only : ncp_to_ndv
    use shape_bezier,           only : map_dv_to_bezier

    double precision,  intent(in)   :: dv (:)
    type(airfoil_type), intent(out) :: foil 

    double precision, allocatable   :: dv_top (:), dv_bot (:)
    type(bezier_spec_type)  :: top_bezier, bot_bezier 
    double precision        :: side_te_gap, le_curv
    integer                 :: ndv_top, npoint

    ! retrieve design variables for top and bot and rebuild bezier control points

    side_te_gap = te_gap (seed_foil) / 2
    ndv_top = ncp_to_ndv(shape_spec%bezier%ncp_top)
    dv_top  = dv (1: ndv_top)

    top_bezier = map_dv_to_bezier (.false., dv_top, side_te_gap)

    if (.not. seed_foil%symmetrical) then
      dv_bot = dv (ndv_top + 1 : )
      ! C2 continuity at LE: derive bot py(2) from top LE curvature
      le_curv = bezier_le_curvature (top_bezier)
      bot_bezier = map_dv_to_bezier (.true., dv_bot, side_te_gap, le_curv)
    else 
      bot_bezier%px =   top_bezier%px
      bot_bezier%py = - top_bezier%py
    end if 

    ! build airfoil coordinates with control points 

    npoint = size(seed_foil%x)
    foil = airfoil_from_bezier (top_bezier, bot_bezier, npoint)

    ! !$omp critical
    ! print * 
    ! write(*,'(A, 10F9.6)') "bez top x", top_bezier%px
    ! write(*,'(A, 10F9.6)') "bez top y", top_bezier%py
    ! write(*,'(A, 10F9.6)') "bez bot x", bot_bezier%px
    ! write(*,'(A, 10F9.6)') "bez bot y", bot_bezier%py
    ! !$omp end critical

  end subroutine create_airfoil_bezier 



  subroutine create_airfoil_bspline (dv, foil) 

    !-----------------------------------------------------------------------------
    !! Create airfoil from B-spline design variables
    !!  - seed is only needed to determine TE gap and number of points 
    !!  - C2 coupling at LE: py(2) of bot is derived from le_curv so that
    !!    curvature at leading edge is identical for top and bot
    !-----------------------------------------------------------------------------

    use airfoil_base,           only : split_foil_into_sides, airfoil_from_bspline, te_gap
    use shape_bspline,          only : bspline_spec_type, bspline_le_curvature
    use shape_bspline,          only : ncp_to_ndv
    use shape_bspline,          only : map_dv_to_bspline

    double precision,  intent(in)   :: dv (:)
    type(airfoil_type), intent(out) :: foil 

    double precision, allocatable   :: dv_top (:), dv_bot (:)
    type(bspline_spec_type) :: top_bspline, bot_bspline 
    double precision        :: side_te_gap, le_curv
    integer                 :: ndv_top, npoint

    ! retrieve design variables for top and bot and rebuild B-spline control points

    side_te_gap = te_gap (seed_foil) / 2
    ndv_top = ncp_to_ndv(shape_spec%bspline%ncp_top)
    dv_top  = dv (1: ndv_top)

    top_bspline = map_dv_to_bspline (.false., dv_top, side_te_gap)

    if (.not. seed_foil%symmetrical) then
      dv_bot = dv (ndv_top + 1 : )
      ! C2 continuity at LE: derive bot py(2) from top LE curvature
      le_curv = bspline_le_curvature (top_bspline)
      bot_bspline = map_dv_to_bspline (.true., dv_bot, side_te_gap, le_curv)
    else 
      bot_bspline%px     =   top_bspline%px
      bot_bspline%py     = - top_bspline%py
    end if 

    ! build airfoil coordinates with control points 

    npoint = size(seed_foil%x)
    foil = airfoil_from_bspline (top_bspline, bot_bspline, npoint)

    ! !$omp critical
    ! print * 
    ! write(*,'(A, 10F9.6)') "bspline top x", top_bspline%px
    ! write(*,'(A, 10F9.6)') "bspline top y", top_bspline%py
    ! write(*,'(A, 10F9.6)') "bspline bot x", bot_bspline%px
    ! write(*,'(A, 10F9.6)') "bspline bot y", bot_bspline%py
    ! !$omp end critical


  end subroutine create_airfoil_bspline 


  
  function get_flap_angles_optimized  (dv) result (flap_angles)
  
    !----------------------------------------------------------------------------
    !! Get actual flap anglesfrom design vars (if there are...) 
    !! If the flap of an op point is fixed, return the fixed value (normally = 0) 
    !! dv:    all design variables! - of flaps are at the end  
    !----------------------------------------------------------------------------
  
    double precision, intent(in)   :: dv (:)
    double precision, allocatable  :: flap_angles (:)
  
    double precision                :: min_angle, max_angle
    integer      :: i, idv, ndv_flaps

  
    ndv_flaps = shape_spec%flap_spec%ndv
    allocate (flap_angles(ndv_flaps))

    ! early exit if there are no flaps optimized 
  
    if (ndv_flaps == 0) return 

    min_angle   = shape_spec%flap_spec%min_flap_angle
    max_angle   = shape_spec%flap_spec%max_flap_angle

    ! retrieve dv of flaps and map them to flap angle 
    
    idv = size(dv) - ndv_flaps  + 1                         ! flap design vars are at the end of dv 
  
    do i = 1, ndv_flaps  
      flap_angles (i) = min_angle + dv(idv) * (max_angle - min_angle)
      idv = idv + 1
    end do 
  
  end function  
  

  
  function get_ndv_of_shape () result (ndv)

    !----------------------------------------------------------------------------
    !! no of designvars of shape depending on shape type
    !----------------------------------------------------------------------------
  
    integer :: ndv
  
    if (shape_spec%type == BEZIER) then
      ndv = shape_bezier_ndv(shape_spec%bezier)
    else if (shape_spec%type == BSPLINE) then
      ndv = shape_bspline_ndv(shape_spec%bspline)
    else if (shape_spec%type == HICKS_HENNE) then                                      
      ndv = shape_hh_ndv(shape_spec%hh)
    else 
      ndv = 0 
    end if
  
  end function 



  function get_ndv_of_flaps () result (ndv)

    !----------------------------------------------------------------------------
    !! no of designvars of flap optimzation (defined in local 'flap_spec') 
    !----------------------------------------------------------------------------
  
    integer :: ndv
  
    ndv = shape_spec%flap_spec%ndv
  
  end function 



  function get_dv0_of_shape () result (dv0)

    !----------------------------------------------------------------------------
    !! designvars for design 0 (equals seed foil) depending on shape type
    !----------------------------------------------------------------------------
  
    use shape_hicks_henne,      only : hh_get_dv0, map_hhs_to_dv
    use shape_bezier,           only : bezier_get_dv0
    use shape_bspline,          only : bspline_get_dv0

    double precision, allocatable :: dv0 (:) 
    integer                       :: ntop, nbot
  
    ! Set initial design
  
    if (shape_spec%type == BEZIER) then
  
      dv0 = [bezier_get_dv0 (is_bot=.false., bezier=seed_foil%top%bezier, c2_coupled=.false.), &
             bezier_get_dv0 (is_bot=.true.,  bezier=seed_foil%bot%bezier, c2_coupled=.true.)]
  
    else if (shape_spec%type == BSPLINE) then
  
      dv0 = [bspline_get_dv0 (is_bot=.false., bspline=seed_foil%top%bspline, c2_coupled=.false.), &
             bspline_get_dv0 (is_bot=.true.,  bspline=seed_foil%bot%bspline, c2_coupled=.true.)]
  
    else if (shape_spec%type == HICKS_HENNE) then                                      
      
      if (is_hh_based (seed_foil)) then                    ! ... then take hhs of seed as inital hhs
        dv0 = [map_hhs_to_dv (seed_foil%top%hh%hhs), &
               map_hhs_to_dv (seed_foil%bot%hh%hhs)]
      else                                               ! ... get "null" hhs - equals seed 
        ntop = shape_spec%hh%nfunctions_top
        nbot = shape_spec%hh%nfunctions_bot
        dv0 = [hh_get_dv0 (ntop), hh_get_dv0 (nbot)]
      end if 
  
    else 
  
      call my_stop ("Unknown shape type")
  
    end if
  
  end function 
  

  
  function get_dv0_of_flaps () result (dv0)
  
    !----------------------------------------------------------------------------
    !! start values of designvariables (0..1) for flaps to be optimized 
    !!    take angle defined in flap spec (from op sepc (input))
    !----------------------------------------------------------------------------
  
    double precision, allocatable :: dv0 (:) 
    double precision              :: min_angle, max_angle, start_angle
    integer                       :: ndv, idv

    ndv = shape_spec%flap_spec%ndv
    allocate (dv0 (ndv))
  
    if (ndv == 0) return                ! no flaps to optimize 
  
    min_angle   = shape_spec%flap_spec%min_flap_angle
    max_angle   = shape_spec%flap_spec%max_flap_angle

    ! map angle array to range 0..1

    do idv = 1, ndv 
      start_angle = shape_spec%flap_spec%start_flap_angle (idv) 
      dv0(idv) = (start_angle - min_angle) / (max_angle - min_angle)
    end do 
      
  end function 
  


  function get_dv_initial_perturb_of_shape () result (dv_perturb)

    !----------------------------------------------------------------------------
    !! inital max. perturb of the designvars for design 0 depending on shape type
    !----------------------------------------------------------------------------
  
    use shape_hicks_henne,      only : hh_get_dv_inital_perturb
    use shape_bezier,           only : bezier_get_dv_inital_perturb
    use shape_bspline,          only : bspline_get_dv_initial_perturb

    double precision, allocatable :: dv_perturb (:)
    integer                       :: ntop, nbot
    double precision              :: initial
  
    ! Set initial design
  
    if (shape_spec%type == BEZIER) then
  
      initial = shape_spec%bezier%initial_perturb
      dv_perturb = [ bezier_get_dv_inital_perturb (initial, seed_foil%top%bezier, c2_coupled=.false.), &
                     bezier_get_dv_inital_perturb (initial, seed_foil%bot%bezier, c2_coupled=.true.)]
  
    else if (shape_spec%type == BSPLINE) then
  
      initial = shape_spec%bspline%initial_perturb
      dv_perturb = [ bspline_get_dv_initial_perturb (initial, seed_foil%top%bspline, c2_coupled=.false.), &
                     bspline_get_dv_initial_perturb (initial, seed_foil%bot%bspline, c2_coupled=.true.)]
  
    else if (shape_spec%type == HICKS_HENNE) then                                      
      
      ! concat dv0 of top and bot side hicks-henne functions 
      ntop = shape_spec%hh%nfunctions_top
      nbot = shape_spec%hh%nfunctions_bot
      initial = shape_spec%hh%initial_perturb

      dv_perturb = [hh_get_dv_inital_perturb (initial, ntop), &
                    hh_get_dv_inital_perturb (initial, nbot)]
  
    else 
  
      call my_stop ("Unknown shape type")
  
    end if
  
  end function 
  

  
  function get_dv_initial_perturb_of_flaps () result (dv_perturb)
  
    !----------------------------------------------------------------------------
    !! initial perturb of flap design vars (for initial design )
    !----------------------------------------------------------------------------
  
    double precision, allocatable :: dv_perturb (:) 
    integer                       :: ndv

    ndv = shape_spec%flap_spec%ndv  
    allocate (dv_perturb (ndv))
  
    if (ndv == 0) return                ! no flaps to optimize 
  
    ! initial perturb 5% of solution space (equals diff of min and max angle) 
    dv_perturb = 0.05d0      
      
  end function 
  


  subroutine debug_print_dv_as_shape_data (iparticle, dv)

    !------------------------------------------------------------------------------
    !! Analysis: Write design variabales either as bezier or hicks henne to dump csv file 
    !------------------------------------------------------------------------------

    use shape_bezier,       only : bezier_spec_type
    use shape_bezier,       only : ncp_to_ndv, map_dv_to_bezier, print_bezier_spec
    use shape_hicks_henne,  only : hh_spec_type, nfunctions_to_ndv, map_dv_to_hhs, print_hh_spec

    integer, intent(in)           :: iparticle
    double precision, intent(in)  :: dv (:) 

    double precision, allocatable   :: dv_shape_spec (:), dv_top(:), dv_bot(:)
    type (bezier_spec_type)         :: top_bezier, bot_bezier
    type (hh_spec_type)             :: top_hh_spec, bot_hh_spec
    integer     :: ndv_top

    !$OMP CRITICAL
    if (shape_spec%type == BEZIER) then
        
      ! dv to bezier shape paramters 

      dv_shape_spec = dv (1 : shape_bezier_ndv(shape_spec%bezier))

      ndv_top    = ncp_to_ndv(shape_spec%bezier%ncp_top)
      dv_top     = dv_shape_spec (1: ndv_top)
      top_bezier = map_dv_to_bezier (.false., dv_top, 0d0)
      call print_bezier_spec (iparticle, TOP, top_bezier) 

      dv_bot     = dv_shape_spec (ndv_top + 1 : )
      bot_bezier = map_dv_to_bezier (.true., dv_bot, 0d0)
      call print_bezier_spec (iparticle, BOT, bot_bezier) 

    else 

      ndv_top = nfunctions_to_ndv (shape_spec%hh%nfunctions_top)
      dv_top = dv (1: ndv_top)
      call map_dv_to_hhs (dv_top, top_hh_spec%hhs)                       ! rebuild hicks henne specs 
      call print_hh_spec (iparticle, TOP,top_hh_spec) 
  
      if (.not. seed_foil%symmetrical) then
  
        dv_bot = dv (ndv_top + 1 : )
        call map_dv_to_hhs (dv_bot, bot_hh_spec%hhs)
        call print_hh_spec (iparticle, BOT,bot_hh_spec) 
    
      end if
    end if

    !$OMP END CRITICAL   
  end subroutine



  subroutine debug_print_vel_as_shape_data (iparticle, vel)

    !------------------------------------------------------------------------------
    !! Analysis: Write velocity vectors in the same top/bot layout as shape data.
    !!           Values are printed in normalized DV space, not mapped to shapes.
    !------------------------------------------------------------------------------

    use shape_bezier,       only : bezier_spec_type
    use shape_bezier,       only : ncp_to_ndv, print_bezier_spec
    use shape_hicks_henne,  only : hh_spec_type, nfunctions_to_ndv, print_hh_spec
    use shape_hicks_henne,  only : hh_type

    integer, intent(in)           :: iparticle
    double precision, intent(in)  :: vel (:) 

    double precision, allocatable :: vel_top(:), vel_bot(:)
    type (hh_spec_type)           :: top_hh_spec, bot_hh_spec
    integer                       :: ndv_top, i, ifunc

    !$OMP CRITICAL
    if (shape_spec%type == BEZIER) then

      ! For Bezier, print the raw normalized vector with the same utility.
      call debug_print_dv_as_shape_data (iparticle, vel)

    else if (shape_spec%type == HICKS_HENNE) then

      ndv_top = nfunctions_to_ndv (shape_spec%hh%nfunctions_top)

      vel_top = vel (1: ndv_top)
      allocate (top_hh_spec%hhs (int(size(vel_top) / 3)))
      i = 1
      do ifunc = 1, size(top_hh_spec%hhs)
        top_hh_spec%hhs(ifunc)%strength = vel_top(i)
        top_hh_spec%hhs(ifunc)%location = vel_top(i+1)
        top_hh_spec%hhs(ifunc)%width    = vel_top(i+2)
        i = i + 3
      end do
      call print_hh_spec (iparticle, TOP, top_hh_spec)

      if (.not. seed_foil%symmetrical) then

        vel_bot = vel (ndv_top + 1 : )
        allocate (bot_hh_spec%hhs (int(size(vel_bot) / 3)))
        i = 1
        do ifunc = 1, size(bot_hh_spec%hhs)
          bot_hh_spec%hhs(ifunc)%strength = vel_bot(i)
          bot_hh_spec%hhs(ifunc)%location = vel_bot(i+1)
          bot_hh_spec%hhs(ifunc)%width    = vel_bot(i+2)
          i = i + 3
        end do
        call print_hh_spec (iparticle, BOT, bot_hh_spec)

      end if

    else

      call debug_print_dv_as_shape_data (iparticle, vel)

    end if
    !$OMP END CRITICAL

  end subroutine debug_print_vel_as_shape_data

end module shape_airfoil