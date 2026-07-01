! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2026 Jochen Guenzel

module airfoil_base

  ! Airfoil type and basic operations 

  use os_util 
  use commons
  use commons,            only : TOP, BOT
  use print_util
  use string_util,        only : stri, strf
  use math_util,          only : point_type, round

  use spline,             only : spline_2D_type, spline_2D
  use shape_bezier,       only : bezier_spec_type, bezier_eval_side, is_bezier_file
  use shape_bspline,      only : bspline_spec_type, bspline_eval_side, write_bspline_file, is_bspline_file
  use shape_hicks_henne,  only : hh_spec_type, load_hh_airfoil, hh_type, map_dv_to_hhs, hh_eval_side, nfunctions_to_ndv, is_hh_file

  implicit none
  private

  double precision, parameter   :: EPSILON = 1.d-8           ! numerical accuracy of geo 

  ! --- main airfoil type ------------------------------------------------------------

  ! Single side of airfoil 

  type side_airfoil_type 
    character(:), allocatable     :: name                ! either 'Top' or 'Bot' or 'thickness' ...
    double precision, allocatable :: x(:)
    double precision, allocatable :: y(:)
    double precision, allocatable :: curvature(:)
    type (bezier_spec_type)       :: bezier              ! bezier curve specification if bezier-based
    type (bspline_spec_type)      :: bspline             ! bspline curve specification if bspline-based
    type (hh_spec_type)           :: hh                  ! hicks-henne specification if hh-based
  end type 

  ! the airfoil type

  type airfoil_type 

    character(:), allocatable     :: filename            ! filename of the airfoil
    character(:), allocatable     :: name                ! name of the airfoil
    double precision, allocatable :: x(:)                ! airfoil coordinates
    double precision, allocatable :: y(:)
    logical :: symmetrical    = .false.                  ! airfoil symmetrical? -> bot equals top side

    type (side_airfoil_type)      :: top                 ! top side of airfoil (includes curve specs)
    type (side_airfoil_type)      :: bot                 ! bottom side of airfoil (includes curve specs)

    type (spline_2D_type)         :: spl                 ! cubic spline of coordinates 

    character (:), allocatable    :: hh_seed_filename    ! filename of seed airfoil hh are applied on 
    double precision, allocatable :: hh_seed_x(:)        ! seed coordinates for rebuild (write)  
    double precision, allocatable :: hh_seed_y(:) 
 
  end type airfoil_type


  type panel_options_type   
    integer          :: npoint                  ! number of coordinate points 
    double precision :: le_bunch                ! panel bunching at le 0..1
    double precision :: te_bunch                ! panel bunching at te 0..1
  end type 

  public :: panel_options_type
  public :: side_airfoil_type
  public :: airfoil_type
  public :: EPSILON
  public :: is_top, is_bot

  ! --- public functions ------------------------------------------------------------

  public :: airfoil_load
  public :: airfoil_load_dat
  public :: airfoil_load_bezier
  public :: airfoil_load_bspline
  public :: airfoil_load_hicks_henne
  public :: airfoil_from_bezier, airfoil_from_bspline, repanel_spline_based
  public :: airfoil_write_dat, airfoil_write_with_shapes
  public :: is_dat_file
  public :: is_bezier_based, is_bspline_based, is_hh_based, is_dat_based, set_as_dat_based
  public :: is_bezier_le_c2
  public :: print_airfoil_write
  public :: build_from_sides, build_from_hh_seed
  public :: split_foil_into_sides, add_suffix_to_name
  public :: is_normalized_coord, is_normalized
  public :: make_symmetrical
  public :: name_flapped_suffix
  public :: create_bezier_example_airfoil, create_bezier_MH30, create_bezier_JX_GS3_100
  public :: le_of_spline, le_of_dat, te_point
  public :: normalize, te_gap
  public :: repanel
  public :: NPOINTS_DEFAULT

  
  double precision, parameter   :: LE_BUNCH_DEFAULT = 0.84d0  ! le bunching for repaneling
  double precision, parameter   :: TE_BUNCH_DEFAULT = 0.6d0   ! te bunching for repaneling
  integer, parameter            :: NPOINTS_DEFAULT = 161      ! default number of points for repaneling


contains


  function airfoil_load (filename) result(foil)

    !-----------------------------------------------------------------------------------
    !! loads either .dat, .bez, .bsp or .hicks file into 'foil' 
    !-----------------------------------------------------------------------------------

    character(*), intent(in)        :: filename
    type(airfoil_type)              :: foil

    if (show_details) then 
      call print_action ("Loading airfoil", filename)
    end if 

    if (is_dat_file (filename)) then 

      foil = airfoil_load_dat (filename)

    else if (is_bezier_file (filename)) then

      foil = airfoil_load_bezier (filename, NPOINTS_DEFAULT)
      
    else if (is_bspline_file (filename)) then

      foil = airfoil_load_bspline (filename, NPOINTS_DEFAULT)   

    else if (is_hh_file (filename)) then

      foil = airfoil_load_hicks_henne (filename)
    
    else

      call my_stop ("Unknown file type: "//quoted (filename))
    
    end if

  end function airfoil_load



  subroutine add_suffix_to_name (foil, suffix, new_basename)

    !! add suffix to airfoil filename and name 

    type(airfoil_type), intent(inout)  :: foil
    character(*), intent(in)           :: suffix
    character(*), intent(in), optional :: new_basename   ! if provided, replaces the base name 
    character(:), allocatable          :: ext

    ext = default_filename_extension(foil)

    foil%filename = ensure_filename_extension(foil%filename, ext)

    if (present(new_basename)) then 
      foil%name     = new_basename
      foil%filename = ensure_filename_extension(new_basename, ext)
    end if

    if (index(foil%name, suffix) == 0) then 
      foil%name = foil%name // suffix
    end if 

    if (index(filename_stem(foil%filename), suffix) == 0) then 
      foil%filename = filename_add_suffix(foil%filename, suffix, ext)
    end if

  end subroutine add_suffix_to_name


  function default_filename_extension (foil) result (ext)

    !! Return default extension according to the current airfoil representation.

    type(airfoil_type), intent(in) :: foil
    character(:), allocatable      :: ext

    if (is_bezier_based(foil)) then
      ext = '.bez'
    else if (is_bspline_based(foil)) then
      ext = '.bsp'
    else if (is_hh_based(foil)) then
      ext = '.hicks'
    else
      ext = '.dat'
    end if

  end function default_filename_extension


  function airfoil_load_bezier (filename, npoint) result(foil)

    !----------------------------------------------------------------------------
    !! Read a Bezier airfoil from a .bez file and return a fully populated foil:
    !!   - foil%x, foil%y       - airfoil coordinates (npoint)
    !!   - foil%top%bezier      - top control points
    !!   - foil%bot%bezier      - bottom control points
    !!   - foil%top, foil%bot   - side_airfoil_type with curvature
    !----------------------------------------------------------------------------

    use shape_bezier,   only : read_bezier_file

    character(*), intent(in)      :: filename
    integer, intent(in)           :: npoint
    type (airfoil_type)           :: foil
    type (bezier_spec_type)       :: top_bezier, bot_bezier
    character (:), allocatable    :: name

    call read_bezier_file (filename, TOP, name, top_bezier)
    call read_bezier_file (filename, BOT, name, bot_bezier)

    foil = airfoil_from_bezier (top_bezier, bot_bezier, npoint)

    foil%name     = name
    foil%filename = filename

  end function



  function airfoil_from_bezier (top_bezier, bot_bezier, npoint) result(foil)

    !----------------------------------------------------------------------------
    !! Build complete airfoil from bezier curve specifications
    !! This centralizes the common pattern:  
    !!   - calculate npoint_top and npoint_bot from npoint
    !!   - evaluate both bezier sides (including analytical curvature)
    !!   - create side_airfoil_type for top and bot
    !!   - combine into full airfoil
    !!   - set bezier specs and flags
    !----------------------------------------------------------------------------

    use spline, only : spline_2D

    type(bezier_spec_type), intent(in)  :: top_bezier, bot_bezier
    integer, intent(in)                 :: npoint
    type(airfoil_type) :: foil

    type(side_airfoil_type) :: top_side, bot_side
    integer :: npoint_top, npoint_bot

    ! Calculate point distribution
    npoint_bot = (npoint + 1) / 2
    npoint_top = (npoint + 1) - npoint_bot

    ! Build top side with analytical curvature
    top_side%name = TOP
    call bezier_eval_side (top_bezier, npoint_top, top_side%x, top_side%y, top_side%curvature)

    ! Build bot side with analytical curvature
    bot_side%name = BOT
    call bezier_eval_side (bot_bezier, npoint_bot, bot_side%x, bot_side%y, bot_side%curvature)

    ! Combine sides into full airfoil
    foil = combine_airfoil_sides (top_side, bot_side)

    ! Build 2D spline for complete airfoil
    foil%spl = spline_2D (foil%x, foil%y)

    ! Set bezier specifications and flag
    foil%top%bezier = top_bezier
    foil%bot%bezier = bot_bezier

  end function



  function airfoil_load_bspline (filename, npoint) result(foil)

    !----------------------------------------------------------------------------
    !! Read a BSpline airfoil from a .bsp file and return a fully populated foil:
    !!   - foil%x, foil%y       - airfoil coordinates (npoint)
    !!   - foil%top%bspline     - top control points
    !!   - foil%bot%bspline     - bottom control points
    !!   - foil%top, foil%bot   - side_airfoil_type with curvature
    !----------------------------------------------------------------------------

    use shape_bspline,   only : read_bspline_file

    character(*), intent(in)      :: filename
    integer, intent(in)           :: npoint
    type(airfoil_type)            :: foil
    type(bspline_spec_type)       :: top_bspline, bot_bspline
    character(:), allocatable     :: name

    call read_bspline_file (filename, TOP, name, top_bspline)
    call read_bspline_file (filename, BOT, name, bot_bspline)

    foil = airfoil_from_bspline (top_bspline, bot_bspline, npoint)

    foil%name     = name
    foil%filename = filename

  end function



  function airfoil_from_bspline (top_bspline, bot_bspline, npoint) result(foil)

    !----------------------------------------------------------------------------
    !! Build complete airfoil from bspline curve specifications
    !! This centralizes the common pattern:
    !!   - calculate npoint_top and npoint_bot from npoint
    !!   - evaluate both bspline sides (including analytical curvature)
    !!   - create side_airfoil_type for top and bot
    !!   - combine into full airfoil
    !!   - set bspline specs and flags
    !----------------------------------------------------------------------------

    use spline, only : spline_2D

    type(bspline_spec_type), intent(in) :: top_bspline, bot_bspline
    integer, intent(in)                 :: npoint
    type(airfoil_type) :: foil

    type(side_airfoil_type) :: top_side, bot_side
    integer :: npoint_top, npoint_bot

    ! Calculate point distribution
    npoint_bot = (npoint + 1) / 2
    npoint_top = (npoint + 1) - npoint_bot

    ! Build top side with analytical curvature
    top_side%name = TOP
    call bspline_eval_side (top_bspline, npoint_top, top_side%x, top_side%y, top_side%curvature)

    ! Build bot side with analytical curvature
    bot_side%name = BOT
    call bspline_eval_side (bot_bspline, npoint_bot, bot_side%x, bot_side%y, bot_side%curvature)

    ! Combine sides into full airfoil
    foil = combine_airfoil_sides (top_side, bot_side)

    ! Build 2D spline for complete airfoil
    foil%spl = spline_2D (foil%x, foil%y)

    ! Set bspline specifications and flag
    foil%top%bspline = top_bspline
    foil%bot%bspline = bot_bspline

  end function


  function airfoil_load_hicks_henne (filename) result(foil)

    !----------------------------------------------------------------------------
    !! Build complete airfoil from Hicks Henne curve specifications
    !! This centralizes the common pattern:
    !!   - evaluate both hh sides (including analytical curvature)
    !!   - create side_airfoil_type for top and bot
    !!   - combine into full airfoil
    !!   - set hh specs and flags
    !----------------------------------------------------------------------------

    character(*), intent(in)       :: filename
    type(airfoil_type) :: foil


    call load_hh_airfoil (filename, foil%name, foil%top%hh, foil%bot%hh, foil%hh_seed_filename, &
                          foil%hh_seed_x, foil%hh_seed_y) 
    foil%filename = filename

    call build_from_hh_seed (foil)

    ! Build 2D spline for complete airfoil
    foil%spl = spline_2D (foil%x, foil%y)


  end function


  function repanel_spline_based (foil_in, panel_options) result(foil)

    !----------------------------------------------------------------------------
    !! Repanel spline-based airfoil with iterative convergence
    !! Iteratively refines existing airfoil until LE converges:
    !!   - finds LE on current spline
    !!   - repanels with cosine distribution
    !!   - normalizes to new LE
    !!   - repeats until LE at origin (converged)
    !----------------------------------------------------------------------------

    use spline,       only : eval_spline, spline_2D
    use math_util,    only : cosine_distribution, norm
    use string_util,  only : stri, strf
    use print_util,   only : print_warning

    type(airfoil_type), intent(in)        :: foil_in
    type(panel_options_type), intent(in)  :: panel_options

    type(spline_2D_type)            :: spl
    type(airfoil_type)              :: foil
    type(point_type)                :: le

    integer                         :: i
    double precision                :: s_le
    double precision, allocatable   :: s(:)

    ! repanel spline also applying new le  

    foil = foil_in

    ! Ensure spline is built before using it
    if (.not. allocated(foil_in%spl%s)) then
      spl = spline_2D (foil_in%x, foil_in%y)
    else
      spl  = foil_in%spl
    end if

    do i = 1, 15

      s_le = le_find_s_of_spline (spl, te_point (foil))   ! find real le of spline
      s    = cosine_panel_distribution (spl, s_le, panel_options)    ! get cosine arc length distribution
      call eval_spline (spl, s, foil%x, foil%y)           ! get new coordinates at repaneled points

      call normalize (foil)                               ! normalize to new le, rebuild complete foil

      spl = foil%spl                                      
      le  = le_of_spline (foil)                           ! find le of repaneled spline  

      if (norm(le) < EPSILON) exit

    end do

    if (norm(le) >= EPSILON) then
      call print_warning ("LE not converged after repaneling: "//strf('f10.8', norm(le)))
    end if  

  end function repanel_spline_based



  subroutine airfoil_points(filename, npoints, labeled)

    !! get number of points from an airfoil file, is there a label?

    character(*), intent(in) :: filename
    integer, intent(out) :: npoints
    logical, intent(out) :: labeled

    integer :: iunit, ioerr
    double precision :: dummyx, dummyz

    ! Open airfoil file

    iunit = 12
    open(unit=iunit, file=filename, status='old', position='rewind', iostat=ioerr)
    if (ioerr /= 0) then
      call my_stop ('Cannot find airfoil file '//trim(filename))
    end if

    ! Read first line; determine if it is a title or not

    read(iunit,*,iostat=ioerr) dummyx, dummyz
    if (ioerr == 0) then
      npoints = 1
      labeled = .false.
    else
      npoints = 0
      labeled = .true.
    end if
    
    ! Read the rest of the lines

    do 
      read(iunit,*,end=500)
      npoints = npoints + 1
    end do

    ! Close the file

    500 close(iunit)

  end subroutine airfoil_points



  function airfoil_load_dat (filename) result (foil)

    !! Read an airfoil from .dat file, automatically determining number of points.
    !! Returns allocatable arrays sized to file contents.
    !! Also checks for incorrect format.

    character(*), intent(in)  :: filename
    type (airfoil_type)       :: foil

    double precision            :: dummy_x, dummy_z
    integer                     :: i, iunit, ioerr, nswitch, npoints
    double precision            :: dir1, dir2
    character(:), allocatable   :: name
    character(512)              :: title_line

    ! Open airfoil file

    iunit = 12
    open(unit=iunit, file=filename, status='old', position='rewind', iostat=ioerr)
    if (ioerr /= 0) then
      call my_stop ('Cannot find airfoil file '//trim(filename))
    end if

    ! First pass - read name and count points

    read(iunit,'(A)', err=2000) title_line
    
    npoints = 0
    do 
      read(iunit,*,end=500, err=1000) dummy_x, dummy_z
      npoints = npoints + 1
    end do

    500 rewind(iunit)

    ! Second pass - allocate and read points from file

    allocate (foil%x(npoints))
    allocate (foil%y(npoints))
  
    read(iunit,'(A)', err=2000) title_line
    name = trim(adjustl(title_line))

    do i = 1, npoints
      read(iunit,*, end=600, err=600) foil%x(i), foil%y(i)
      foil%x(i) = foil%x(i) + 0d0                             ! get rid of -0d0
      foil%y(i) = foil%y(i) + 0d0 
    end do

    close(iunit)

    ! Check that coordinates are formatted correctly

    nswitch = 0
    dir1 = foil%x(2) - foil%x(1)
    do i = 3, npoints
      dir2 = foil%x(i) - foil%x(i-1)
      if (dir2 /= 0.d0) then
        if (dir2*dir1 < 0.d0) nswitch = nswitch + 1
        dir1 = dir2
      end if
    end do

    if (nswitch /= 1) then
      call my_stop ("Incorrect .dat file format - airfoil not properly ordered in "//trim(filename))
    end if

    ! all read - finalize

    foil%name     = name
    foil%filename = filename

    call split_foil_into_sides (foil)

    return

    ! error handling

    600 close(iunit)
    call my_stop ("Incorrect .dat file format at line "//stri(i)//" in "//trim(filename))

    1000 close(iunit)
    call my_stop ("Error reading data points from "//trim(filename))

    2000 close(iunit)
    call my_stop ("Cannot read airfoil name from file "//trim(filename))

  end function airfoil_load_dat



  function is_dat_file (filename)

    !! .true. if filename has ending '.dat'

    character(*),  intent(in) :: filename
    logical                   :: is_dat_file 
    character(:), allocatable :: suffix 
    
    suffix = filename_extension (filename)
    is_dat_file = suffix == '.dat' .or. suffix =='.DAT'

  end function  


  function is_bezier_based (foil) result(is_bez)

    !! is airfoil based on Bezier curves? Checks if bezier control points are allocated.

    type(airfoil_type), intent(in) :: foil
    logical :: is_bez

    is_bez = allocated(foil%top%bezier%px) .and. allocated(foil%bot%bezier%px)

  end function is_bezier_based


  function is_bezier_le_c2 (foil) result(is_le_c2)

    !! is airfoil based on Bezier curves with C2 continuity at LE? 

    use shape_bezier, only : bezier_le_curvature

    type(airfoil_type), intent(in) :: foil

    logical :: is_le_c2
    double precision :: le_curv_top, le_curv_bot

    if (.not. is_bezier_based (foil)) then 
      is_le_c2 = .false.
      return
    end if

    le_curv_top = bezier_le_curvature (foil%top%bezier)
    le_curv_bot = bezier_le_curvature (foil%bot%bezier)

    is_le_c2 = round (le_curv_top, 6) == round (le_curv_bot, 6)

  end function is_bezier_le_c2


  function is_bspline_based (foil) result(is_bsp)

    !! is airfoil based on BSpline curves? Checks if bspline control points are allocated.

    type(airfoil_type), intent(in) :: foil
    logical :: is_bsp

    is_bsp = allocated(foil%top%bspline%px) .and. allocated(foil%bot%bspline%px)

  end function is_bspline_based


  function is_hh_based (foil) result(is_hh)

    !! is airfoil based on Hicks Henne functions? Checks if hh specs are allocated.

    type(airfoil_type), intent(in) :: foil
    logical :: is_hh

    is_hh = allocated(foil%top%hh%hhs) .and. allocated(foil%bot%hh%hhs)

  end function is_hh_based


  function is_dat_based (foil) result(is_dat)

    !! is airfoil based on .dat file? Checks if neither bezier nor bspline nor hh specs are allocated.

    type(airfoil_type), intent(in) :: foil
    logical :: is_dat

    is_dat = .not. is_bezier_based(foil) .and. .not. is_bspline_based(foil) .and. .not. is_hh_based(foil)

  end function is_dat_based

  subroutine set_as_dat_based (foil)

    !! set airfoil as .dat based by deallocating bezier and bspline specs

    type(airfoil_type), intent(inout) :: foil

    if (is_bezier_based (foil)) then 
      deallocate(foil%top%bezier%px)
      deallocate(foil%bot%bezier%px)
    end if 

    if (is_bspline_based (foil)) then 
      deallocate(foil%top%bspline%px)
      deallocate(foil%bot%bspline%px)
    end if 

    if (is_hh_based (foil)) then 
      deallocate(foil%top%hh%hhs)
      deallocate(foil%bot%hh%hhs)
    end if 

    ! split into sides to update curvature and ensure consistency
    call split_foil_into_sides (foil)   

  end subroutine set_as_dat_based 


  function is_normalized_coord (foil) result(is_norm)

    !! Checks if foil is normalized - only looking at coordinates (no real LE check)
    !!  - Leading edge at 0,0 
    !!  - Trailing edge at 1,0 (upper and lower side may have a gap) 

    type(airfoil_type), intent(in)  :: foil
    logical       :: is_norm
    integer       :: ile

    is_norm = .true. 

    ! Check TE 

    if (foil%x(1) /= 1d0 .or. foil%x(size(foil%x)) /= 1d0)    is_norm = .false.  
    if ((foil%y(1) + foil%y(size(foil%x))) /= 0d0)            is_norm = .false.

    ! Check LE 

    ile = (minloc (foil%x,1))
    if (foil%x(ile) /= 0d0)                                   is_norm = .false.
    if (foil%y(ile) /= 0d0)                                   is_norm = .false.

  end function is_normalized_coord



  function is_normalized (foil) result(is_norm)

    !! Checks if foil is normalized 
    !!  - Leading edge real and virtual at 0,0 
    !!  - Trailing edge at 1,0 (upper and lower side may have a gap) 

    use math_util,      only : point_type, norm
    use spline,         only : spline_2D

    type(airfoil_type), intent(in)  :: foil

    logical                     :: is_norm
    type(airfoil_type)          :: foil_splined
    type (point_type)           :: le_s
    double precision, parameter :: EPSILON_LE_CLOSE = 1d-6

    is_norm = is_normalized_coord (foil)
    if (.not. is_norm) return 

    ! sanity check - spline is needed for find the real, splined LE

    foil_splined = foil                                     ! foil is just input

    if (.not. allocated(foil%spl%s)) then
      foil_splined%spl = spline_2D (foil%x, foil%y)
    end if 

    ! Get leading edge location of spline and .dat

    le_s = le_of_spline (foil_splined)

    is_norm = (norm (le_s) <= EPSILON_LE_CLOSE)

  end function is_normalized



  function combine_airfoil_sides (top_side, bot_side) result(foil)

    !-----------------------------------------------------------------------------
    !! Combine top and bot side into full airfoil
    !! Both sides are assumed to go from LE to TE
    !! Result: full airfoil with complete coordinates, sides, no spline populated!
    !-----------------------------------------------------------------------------

    use spline, only : spline_2D

    type(side_airfoil_type), intent(in) :: top_side, bot_side
    type(airfoil_type) :: foil

    integer :: npoint_top, npoint_bot, npoint

    npoint_top = size(top_side%x)
    npoint_bot = size(bot_side%x)
    npoint = npoint_top + npoint_bot - 1

    allocate (foil%x(npoint))
    allocate (foil%y(npoint))

    ! top side reversed (TE to LE)
    foil%x(1:npoint_top) = top_side%x(npoint_top:1:-1)
    foil%y(1:npoint_top) = top_side%y(npoint_top:1:-1)

    ! bot side forward (LE to TE), skip LE point to avoid duplicate
    foil%x(npoint_top+1:npoint) = bot_side%x(2:npoint_bot)
    foil%y(npoint_top+1:npoint) = bot_side%y(2:npoint_bot)

    ! Build 2D spline for the complete airfoil
    ! foil%spl = spline_2D (foil%x, foil%y)

    ! Store side references
    foil%top = top_side
    foil%bot = bot_side

  end function combine_airfoil_sides




  subroutine split_foil_into_sides (foil)

    !-----------------------------------------------------------------------------
    !! Split an airfoil into its top and bottom side
    !! if there is already a leading edge at 0,0 
    !-----------------------------------------------------------------------------

    use spline,       only : spline_2d, eval_spline_curvature
 
    type(airfoil_type), intent(inout) :: foil
    double precision, allocatable     :: curv (:) 
    integer               :: ile

    ! if (.not. is_normalized_coord (foil)) then 
    !     le = le_of_dat (foil)
    !     call print_warning ("split_foil: Airfoil not normalized (LE: "// & 
    !                         strf('f19.7', le%x)//','//strf('f19.7',  le%y)//")")
    ! end if  

    !! build 2D spline 

    foil%spl = spline_2D (foil%x, foil%y)

    ! get curvature of complete surface

    curv = eval_spline_curvature (foil%spl, foil%spl%s) 
    
    ! split the polylines

    ile = minloc (foil%x, 1)

    foil%top%name = TOP
    foil%top%x = foil%x(iLe:1:-1)
    foil%top%y = foil%y(iLe:1:-1)
    foil%top%curvature = curv(iLe:1:-1)

    foil%bot%name = BOT

    if (.not. foil%symmetrical) then

      foil%bot%x = foil%x(iLe:)
      foil%bot%y = foil%y(iLe:)
      foil%bot%curvature = curv(iLe:)

    else                                     ! just sanity - it should already be symmetrical
      
      foil%bot%x =  foil%top%x
      foil%bot%y = -foil%top%y
      foil%bot%curvature = foil%top%curvature

    end if 

  end subroutine 



  subroutine build_from_hh_seed (foil)

    !-----------------------------------------------------------------------------
    !! (re)builds foil out of its seed x,y and its hh functions 
    !-----------------------------------------------------------------------------

    type(airfoil_type), intent(inout) :: foil 

    integer                         :: ile
    double precision, allocatable   :: seed_top_x (:), seed_top_y (:), top_y_new (:)
    double precision, allocatable   :: seed_bot_x (:), seed_bot_y (:), bot_y_new (:)

    ! split the seed airfoil

    ile = minloc (foil%hh_seed_x, 1)

    seed_top_x = foil%hh_seed_x(iLe:1:-1)
    seed_top_y = foil%hh_seed_y(iLe:1:-1)
    top_y_new  = seed_top_y
    call hh_eval_side (foil%top%hh, seed_top_x, top_y_new )   ! and add hicks hennes to y 

    if (.not. foil%symmetrical) then
      seed_bot_x = foil%hh_seed_x(iLe:)
      seed_bot_y = foil%hh_seed_y(iLe:)
      bot_y_new  = seed_bot_y
      call hh_eval_side (foil%bot%hh, seed_bot_x, bot_y_new ) ! and add hicks hennes to y 

    else                                                      ! just sanity - it should already be symmetrical
      seed_bot_x = seed_top_x
      seed_bot_y = -seed_top_y
      bot_y_new  = -top_y_new
    end if 

    ! build foil out of new top and bot side 

    foil%top%x  = seed_top_x
    foil%top%y  = top_y_new
    foil%bot%x  = seed_bot_x
    foil%bot%y  = bot_y_new

    call build_from_sides (foil)

  end subroutine 



  subroutine build_from_sides (foil)

    !-----------------------------------------------------------------------------
    !! rebuild foil from its current top and bot side - recalc curvature of top and bot 
    !-----------------------------------------------------------------------------

    use spline, only : spline_2D, eval_spline_curvature

    type(airfoil_type), intent(inout)     :: foil

    double precision, allocatable         :: curv (:) 
    integer   :: npt, npb, np

    npt = size(foil%top%x)
    npb = size(foil%bot%x)
    np  = npt + npb - 1

    if (allocated(foil%x)) deallocate(foil%x)
    if (allocated(foil%y)) deallocate(foil%y)
    allocate(foil%x(np))
    allocate(foil%y(np))

    foil%x(1:npt) = foil%top%x (npt:1:-1)
    foil%y(1:npt) = foil%top%y (npt:1:-1)

    foil%x(npt:)  = foil%bot%x 
    foil%y(npt:)  = foil%bot%y  

    foil%top%name = TOP
    foil%bot%name = BOT
   
    ! rebuild spline, get curvature 

    foil%spl = spline_2D (foil%x, foil%y)
    curv = eval_spline_curvature (foil%spl, foil%spl%s)

    foil%top%curvature = curv(npt:1:-1)
    foil%bot%curvature = curv(npt:)

  end subroutine 




  subroutine make_symmetrical (foil)

    !-----------------------------------------------------------------------------
    !! mirrors top side to bot to make foil symmetrical
    !-----------------------------------------------------------------------------

    type(airfoil_type), intent(inout) :: foil
    integer ile

    call print_note ("Mirroring top half of seed airfoil for symmetrical constraint.")

    ile = minloc (foil%x, 1)
    if (ile == 0 .or. foil%x(ile) /= 0d0 .or. foil%y(ile) /= 0d0) then 
      call my_stop ("make_symmetrical: Leading edge isn't at 0,0")
    end if  

    foil%bot%x =  foil%top%x
    foil%bot%y = -foil%top%y
    foil%symmetrical = .true.

    call build_from_sides (foil)

    if (is_bezier_based (foil)) then
      foil%bot%bezier%px =  foil%top%bezier%px 
      foil%bot%bezier%py = -foil%top%bezier%py
    else if (is_bspline_based (foil)) then
      foil%bot%bspline%px =  foil%top%bspline%px 
      foil%bot%bspline%py = -foil%top%bspline%py
    end if 

  end subroutine 



  subroutine airfoil_write_dat (pathFileName, name, x,y)
     
    !-----------------------------------------------------------------------------
    !! Writes an airfoil to a labeled file
    !-----------------------------------------------------------------------------
    
    character(*), intent(in)        :: pathFileName, name
    double precision, intent(in)    :: x(:), y(:)

    integer                         :: iunit, ioerr, i
    character(:), allocatable       :: fileName
    character(len=512)              :: msg


    fileName = trim(pathFileName)
    if (.not. is_dat_file(fileName)) then
      fileName = filename_replace_extension(fileName, '.dat')
    end if

    ! Open file for writing and out ...

    iunit = 13
    open  (unit=iunit, file=fileName, status='replace',  iostat=ioerr, iomsg=msg)
    if (ioerr /= 0) then 
      call my_stop ("Unable to write to file '"//trim(fileName)//"': "//trim(msg))
    end if 

    write(iunit,'(A)') name

    ! Write coordinates

    do i = 1, size(x)
      write(iunit,'(2F12.7)') x(i), y(i)
    end do

    close (iunit)

  end subroutine 



  subroutine print_airfoil_write (dir, fileName, highlight)
     
    !-----------------------------------------------------------------------------
    !! print user message about writing an airfoil 
    !! If 'highlight' the airfoil name will be highlighted
    !-----------------------------------------------------------------------------
    
    character(*), intent(in)        :: dir, fileName
    logical, intent(in), optional   :: highlight 

    character (:), allocatable      :: file_type
    logical                         :: do_highlight

    file_type = filename_extension (fileName)

    if (present(highlight)) then 
      do_highlight = highlight 
    else 
      do_highlight = .true. 
    end if 

    if (.not. show_details .and. .not. do_highlight) return 

    if (file_type == "bez") then 
      call print_action ("Writing bezier      ", no_crlf = .true.)
    else if (file_type == "bsp") then 
      call print_action ("Writing bspline     ", no_crlf = .true.)
    else if (file_type == "hicks") then 
      call print_action ("Writing hicks-henne ", no_crlf = .true.)
    else
      call print_action ("Writing airfoil     ", no_crlf = .true.)
    end if 

    if (do_highlight) then 
      call print_colored (COLOR_NORMAL, fileName)
    else 
      call print_colored (COLOR_NOTE,   fileName)
    end if 

    if (dir == "") then 
      print * 
    else 
      call print_text ("to "//dir)
    end if  


  end subroutine 



  subroutine airfoil_write_with_shapes (foil, output_dir, highlight)

    !-----------------------------------------------------------------------------
    !! write airfoil .dat and bezier or hicks henne files 
    !! optional print airfoil name highlighted (default) 
    !-----------------------------------------------------------------------------

    use shape_bezier,       only : write_bezier_file
    use shape_hicks_henne,  only : write_hh_file
 
    type (airfoil_type), intent(in) :: foil 
    character (*), intent(in)       :: output_dir 
    logical, intent(in), optional   :: highlight 

    character (:), allocatable      :: fileName, name, normalized_file
    logical                         :: do_highlight

    if (present(highlight)) then 
      do_highlight = highlight 
    else 
      do_highlight = .true. 
    end if 

    normalized_file = ensure_filename_extension(foil%filename, default_filename_extension(foil))

    name     = foil%name
    if (trim(name) == "") then 
      name = filename_stem (normalized_file)
    end if

    ! write normal .dat 

    fileName = filename_replace_extension(normalized_file, '.dat')
    call print_airfoil_write (output_dir, fileName, highlight=do_highlight)
    call airfoil_write_dat   (path_join (output_dir, fileName), name, foil%x, foil%y)
    
    ! write bezier .bez 

    if (is_bezier_based (foil)) then

      fileName = filename_replace_extension(normalized_file, '.bez')
      call print_airfoil_write (output_dir, fileName, highlight=do_highlight)
      call write_bezier_file   (path_join (output_dir, fileName), name, foil%top%bezier, foil%bot%bezier)
    
    ! write bspline .bsp 

    else if (is_bspline_based (foil)) then

      fileName = filename_replace_extension(normalized_file, '.bsp')
      call print_airfoil_write (output_dir, fileName, highlight=do_highlight)
      call write_bspline_file  (path_join (output_dir, fileName), name, foil%top%bspline, foil%bot%bspline)

    ! write hicks-henne .hicks 

    else if (is_hh_based (foil)) then

      fileName = filename_replace_extension(normalized_file, '.hicks')
      call print_airfoil_write (output_dir, fileName, highlight=do_highlight)
      call write_hh_file (path_join (output_dir, fileName), name, foil%top%hh, foil%bot%hh, &
                          foil%hh_seed_filename, foil%hh_seed_x, foil%hh_seed_y)

    end if 
  
  end subroutine 


  function name_flapped_suffix (angle) result (suffix) 
     
    !-----------------------------------------------------------------------------
    !! returns suffix for airfoil being flapped with angle (in degrees)
    !-----------------------------------------------------------------------------
    
    double precision, intent(in)        :: angle
    character(:), allocatable           :: suffix
    character (20)                      :: text_degrees

    if (angle /= 0) then 

      if (int(angle)*10  == int(angle*10d0)) then       !degree having decimal?
        write (text_degrees,'(SP,I3)') int (angle)
      else
        write (text_degrees,'(SP,F6.1)') angle
      end if
      suffix = '_f' // trim(adjustl(text_degrees))

    else
      suffix = ''
    end if 

  end function name_flapped_suffix



!=============================================================================80
!
! Example bezier airfoil creation functions - hardcoded control points
!
!=============================================================================80

  function create_bezier_example_airfoil (npoint) result(foil)
    !! create simple example bezier airfoil

    integer, intent(in)  :: npoint
    type(airfoil_type) :: foil

    type(bezier_spec_type) :: top_bezier, bot_bezier

    top_bezier%px = [   0d0,    0d0, 0.33d0,  1d0]
    top_bezier%py = [   0d0, 0.06d0, 0.12d0,  0d0]

    bot_bezier%px = [   0d0,    0d0, 0.25d0,  1d0]
    bot_bezier%py = [   0d0,-0.04d0,-0.07d0,  0d0]  

    foil      = airfoil_from_bezier (top_bezier, bot_bezier, npoint)
    foil%filename = ensure_filename_extension ("bezier_example", '.bez')
    foil%name     = "bezier_example"

  end function


  function create_bezier_MH30 (npoint) result(foil)
    !! create MH30-norm-bezier - data from Airfoil Editor match bezier 

      ! .\test-cases\reference-airfoils\MH 30-norm-bezier-7-5.bez


      ! MH 30-norm-bezier-7-5
      ! Top Start
      !  0.0000000000  0.0000000000
      !  0.0000000000  0.0143951798
      !  0.1032405700  0.1029764415
      !  0.4901254031  0.0382743125
      !  0.6314525508  0.0661920564
      !  0.8552401081  0.0130084982
      !  1.0000000000  0.0000000000
      ! Top End
      ! Bottom Start
      !  0.0000000000  0.0000000000
      !  0.0000000000 -0.0048568117
      !  0.0130325103 -0.0587325332
      !  0.5995297333 -0.0043740872
      !  1.0000000000  0.0000000000
      ! Bottom End
    

    integer, intent(in)  :: npoint 
    type(airfoil_type) :: foil

    type(bezier_spec_type) :: top_bezier, bot_bezier

    top_bezier%px = [0.0000000000d0, 0.0000000000d0, 0.1032405700d0, 0.4901254031d0, &
                     0.6314525508d0, 0.8552401081d0, 1.0000000000d0]
    top_bezier%py = [0.0000000000d0, 0.0143951798d0, 0.1029764415d0, 0.0382743125d0, &
                     0.0661920564d0, 0.0130084982d0, 0.0000000000d0]

    bot_bezier%px = [0.0000000000d0, 0.0000000000d0, 0.0130325103d0, 0.5995297333d0, 1.0000000000d0]
    bot_bezier%py = [0.0000000000d0,-0.0048568117d0,-0.0587325332d0,-0.0043740872d0, 0.0000000000d0]  

    foil = airfoil_from_bezier (top_bezier, bot_bezier, npoint)
    foil%filename = ensure_filename_extension ("MH30-norm-bezier", '.bez')
    foil%name     = "MH30-norm-bezier"

  end function


  function create_bezier_JX_GS3_100 (npoint) result(foil)
    !! create JX-GS3-norm-bezier - data from Airfoil Editor match bezier 


    ! JX-GS3-100_v6
    ! Top Start
    !   0.0000000000  0.0000000000
    !   0.0000000000  0.0100289826
    !   0.0465503579  0.0991544063
    !   0.5954343392  0.0447847058
    !   0.6884470789  0.0387149830
    !   1.0000000000  0.0001467000
    ! Top End
    ! Bottom Start
    !   0.0000000000  0.0000000000
    !   0.0000000000 -0.0196099710
    !   0.1769273284 -0.0388186178
    !   0.4836067048 -0.0193950580
    !   0.7644006593 -0.0091005078
    !   1.0000000000 -0.0001467000
    ! Bottom End

    
    integer, intent(in)  :: npoint
    type(airfoil_type) :: foil

    type(bezier_spec_type) :: top_bezier, bot_bezier

    top_bezier%px = [0.0000000000d0, 0.0000000000d0, 0.0465503579d0, 0.5954343392d0, &
                     0.6884470789d0, 1.0000000000d0]
    top_bezier%py = [0.0000000000d0, 0.0100289826d0, 0.0991544063d0, 0.0447847058d0, &
                     0.0387149830d0, 0.0001467000d0]

    bot_bezier%px = [0.0000000000d0, 0.0000000000d0, 0.1769273284d0, 0.4836067048d0, &
                     0.7644006593d0, 1.0000000000d0]
    bot_bezier%py = [0.0000000000d0,-0.0196099710d0,-0.0388186178d0,-0.0193950580d0, &
                     -0.0091005078d0,-0.0001467000d0]

    foil = airfoil_from_bezier (top_bezier, bot_bezier, npoint)
    foil%filename = ensure_filename_extension ("JX-GS3-100_v6", '.bez')
    foil%name     = "JX-GS3-100_v6"

  end function



  function le_find_s_of_spline (spl, te) result (sle)

    !----------------------------------------------------------------------------
    !! Find leading edge arc length on a spline using Newton iteration
    !! LE is defined as the point where tangent is perpendicular to TE chord
    !! Samples spline to find best initial guess (minimum x)
    !----------------------------------------------------------------------------

    use math_util, only : clip
    use spline,    only : eval_spline

    type (spline_2D_type), intent(in) :: spl
    type (point_type), intent(in)     :: te

    double precision    :: sle, x, y, dx, dy, ddx, ddy, dot, ddot
    double precision    :: dxTe, dyTe, ds, s_start, s_end
    double precision    :: s_search_min, s_search_max, s_le_guess
    double precision, allocatable :: x_array(:), y_array(:)
    integer :: iter, ile
    double precision, parameter :: EPS = 1d-10
    double precision, parameter :: SEARCH_WINDOW = 0.1d0  ! Search within ±10% around guess


    ! Sample spline at all arc length points to find s with minimum x (vectorized)
    call eval_spline (spl, spl%s, x_array, y_array, 0)
    ile = minloc(x_array, 1)
    s_le_guess = spl%s(ile)

    ! Initial guess
    sle = s_le_guess
    
    ! Define local search window around guess to prevent surface crossing
    s_start = spl%s(1)
    s_end   = spl%s(size(spl%s))
    s_search_min = max(s_start, s_le_guess - SEARCH_WINDOW * (s_end - s_start))
    s_search_max = min(s_end,   s_le_guess + SEARCH_WINDOW * (s_end - s_start))

    ! Newton iteration to find LE (constrained to local window)
    do iter = 1, 50

      sle = clip (sle, s_search_min, s_search_max)
      call eval_spline (spl, sle, x, y, 0)
      call eval_spline (spl, sle, dx, dy, 1)
      dxTe = x - te%x
      dyTe = y - te%y
      dot = dx * dxTe + dy * dyTe

      if ((abs(dot) < EPS)) exit

      call eval_spline (spl, sle, ddx, ddy, 2)
      ddot = dx**2 + dy**2 + dxTe * ddx + dyTe * ddy
      ds = - dot / ddot
      
      ! Limit step size to avoid huge jumps when ddot is small
      ds = clip (ds, -0.1d0*(s_end - s_start), 0.1d0*(s_end - s_start))
      sle = sle + ds

    end do

    ! Check convergence - if failed, use initial guess and warn
    if (abs(dot) >= EPS) then
      call print_warning ("Newton iteration for LE did not converge, using LE of data points as guess")
      sle = s_le_guess
    end if

  end function


  function le_of_spline (foil) result (le_point)

    !----------------------------------------------------------------------------
    !! Find leading edge point from spline
    !! Returns le point where tangent is perpendicular to TE chord
    !----------------------------------------------------------------------------

    use spline, only : eval_spline

    type (airfoil_type), intent(in) :: foil
    type (point_type) :: le_point

    double precision :: sle

    ! Find exact LE using Newton iteration
    sle = le_find_s_of_spline (foil%spl, te_point(foil))
    call eval_spline (foil%spl, sle, le_point%x, le_point%y, 0)

  end function


  function le_of_dat (foil) result (le_point)

    !----------------------------------------------------------------------------
    !! Find leading edge point from .dat coordinates
    !! Returns le point where x is minimum 
    !----------------------------------------------------------------------------

    type (airfoil_type), intent(in) :: foil
    type (point_type) :: le_point
    integer :: ile

    ile = minloc (foil%x, 1)
    le_point%x = foil%x(ile)
    le_point%y = foil%y(ile)

  end function



  function te_point (foil) result (te)

    !! Get trailing edge point as midpoint of TE coordinates

    type(airfoil_type), intent(in)  :: foil
    type(point_type) :: te
    integer :: n

    n = size(foil%x)
    te%x = (foil%x(1) + foil%x(n)) / 2d0
    te%y = (foil%y(1) + foil%y(n)) / 2d0

  end function


  function te_gap (foil)

    !! trailing edge gap of foil 

    type(airfoil_type), intent(in)  :: foil
    double precision :: te_gap
  
    te_gap = sqrt ((foil%x(1) - foil%x(size(foil%x)))**2 + &
                   (foil%y(1) - foil%y(size(foil%y)))**2)
  end function




  subroutine normalize (foil, basic)

    !----------------------------------------------------------------------------
    !! Translates and scales an airfoil 
    !! If 'basic' then LE of coordinates is taken - otherwise LE of spline 
    !! - length of 1 
    !! - leading edge at 0,0 and trailing edge is symmetric at 1,x
    !----------------------------------------------------------------------------

    use spline, only : spline_2D

    type(airfoil_type), intent(inout) :: foil
    logical, intent(in), optional     :: basic 

    double precision    :: foilscale_upper, foilscale_lower
    double precision    :: angle, cosa, sina
    double precision    :: xi, yi, te_gap_old
    type (point_type)   :: le

    integer :: npoints, i, ile
    logical :: just_basic

    npoints = size(foil%x)
    te_gap_old = te_gap (foil)

    ! basic normalize or based on spline? 

    if (present(basic)) then 
      just_basic = basic 
    else 
      just_basic = .true.
    end if 

    if (just_basic) then 
      le = le_of_dat (foil)
    else
      le = le_of_spline (foil)
    end if 

    ! Translate so that the leading edge is at the origin

    do i = 1, npoints
      foil%x(i) = foil%x(i) - le%x
      foil%y(i) = foil%y(i) - le%y
    end do

    ! Rotate the airfoil so chord is on x-axis 

    angle = atan2 ((foil%y(1)+foil%y(npoints))/2.d0,(foil%x(1)+foil%x(npoints))/2.d0)
    cosa  = cos (-angle) 
    sina  = sin (-angle) 
    do i = 1, npoints
      xi = foil%x(i) 
      yi = foil%y(i)
      foil%x(i) = xi * cosa - yi * sina
      foil%y(i) = xi * sina + yi * cosa
    end do

    ! Ensure TE is at x=1

    If (foil%x(1) /= 1d0) then 

      ! Scale airfoil so that it has a length of 1 
      ! - there are mal formed airfoils with different TE on upper and lower
      ! - also from rotation there is a mini diff  

      ile = minloc (foil%x, 1)
      foilscale_upper = 1.d0 / foil%x(1)
      do i = 1, ile  ! - 1
        foil%x(i) = foil%x(i)*foilscale_upper
        foil%y(i) = foil%y(i)*foilscale_upper
      end do

    end if 

    If (foil%x(npoints) /= 1d0) then 
      ile = minloc (foil%x, 1)
      foilscale_lower = 1.d0 / foil%x(npoints)
      do i = ile + 1, npoints
          foil%x(i) = foil%x(i)*foilscale_lower
          foil%y(i) = foil%y(i)*foilscale_lower
      end do
    end if 

    foil%x(1)       = 1d0                                   ! ensure now really, really
    foil%x(npoints) = 1d0

    ! Force TE to old TE gap if delta < epsilon 

    if (abs(foil%y(1)) < EPSILON) then 
      foil%y(1)       = 0d0                     ! make te gap to 0.0
      foil%y(npoints) = 0d0 
    else if (abs(foil%y(1) - (te_gap_old/2d0)) < EPSILON) then 
      foil%y(1)       =  te_gap_old/2d0 
      foil%y(npoints) = -te_gap_old/2d0 
    end if 

    ! rebuild spline 

    foil%spl = spline_2D (foil%x, foil%y)

    ! create sides

    call split_foil_into_sides (foil)

  end subroutine normalize


  function cosine_panel_distribution (spl_in, s_le, panel_options) result(s)

    !-----------------------------------------------------------------------------
    !! Create cosine-distributed arc length array for repaneling
    !! Returns arc length s(:) with cosine distribution, bunching at LE and TE
    !-----------------------------------------------------------------------------

    use math_util,        only : cosine_distribution
    use spline,           only : spline_2D, eval_spline

    type(spline_2D_type), intent(in) :: spl_in
    double precision, intent(in)     :: s_le
    type(panel_options_type), intent(in)  :: panel_options

    double precision, allocatable :: s(:), s_top(:), s_bot(:), u_top(:), u_bot(:)
    integer                       :: npoint, nPanels, nPan_top, nPan_bot
    double precision              :: s_start, s_end

    npoint  = panel_options%npoint
    nPanels = npoint - 1

    ! in case of odd number of panels, top side will have +1 panels 
    if (mod(nPanels,2) == 0) then
        nPan_top = int (nPanels / 2)
        nPan_bot = nPan_top
    else 
        nPan_bot = int(nPanels / 2)
        nPan_top = nPan_bot + 1 
    end if 

    ! Find major arc length points
    s_start = spl_in%s(1) 
    s_end   = spl_in%s(size(spl_in%s))

    ! normalized point distribution u 

    u_top = cosine_distribution (nPan_top+1, panel_options%le_bunch, panel_options%te_bunch)
    u_top = u_top (size(u_top) : 1 : -1)        ! flip
    s_top = s_start + abs (u_top - 1d0) * s_le
    s_top(size(s_top)) = s_le                   ! Force exact LE point

    u_bot = cosine_distribution (nPan_bot+1, panel_options%le_bunch, panel_options%te_bunch)
    s_bot = s_le + u_bot * (s_end - s_le)
    s_bot(1) = s_le                             ! Force exact LE point

    ! add new top and bot distributions 

    s = [s_top, s_bot(2:)]  

  end function cosine_panel_distribution


  function repanel (foil_in, panel_options, silent) result(foil)

    !-----------------------------------------------------------------------------
    !! Unified repanel function - handles all shape types (bezier, bspline, spline)
    !! Dispatches to appropriate airfoil_from_* function based on shape type
    !-----------------------------------------------------------------------------

    type(airfoil_type), intent(in)        :: foil_in
    type(panel_options_type), intent(in), optional  :: panel_options
    logical, intent(in), optional         :: silent
    type(airfoil_type)                    :: foil

    type(panel_options_type)        :: panel_options_local
    logical                         :: do_print

    ! use default panel options if not provided 

    if (present (panel_options)) then 
      panel_options_local = panel_options
    else 
      panel_options_local%npoint   = NPOINTS_DEFAULT
      panel_options_local%le_bunch = LE_BUNCH_DEFAULT
      panel_options_local%te_bunch = TE_BUNCH_DEFAULT
    end if

    ! Print message unless explicitly silenced
    do_print = .true.
    if (present(silent)) do_print = .not. silent
    if (do_print) call print_action ('Repaneling - airfoil will have ', stri(panel_options_local%npoint) //' Points')

    ! Dispatch based on shape type

    if (is_bezier_based(foil_in)) then
      ! Bezier-based: use bezier curves with arc-length distribution
      foil = airfoil_from_bezier (foil_in%top%bezier, foil_in%bot%bezier, panel_options_local%npoint)
    
    else if (is_bspline_based(foil_in)) then
      ! B-spline-based: use bspline curves with arc-length distribution
      foil = airfoil_from_bspline (foil_in%top%bspline, foil_in%bot%bspline, panel_options_local%npoint)
    
    else
      ! Spline-based: iterative repanel with convergence
      foil      = repanel_spline_based (foil_in, panel_options_local)
    
    end if

    foil%name     = foil_in%name
    foil%filename = foil_in%filename
    call add_suffix_to_name (foil, '-repan')

  end function repanel



  pure elemental function is_top(side) result(top_side)

    !-----------------------------------------------------------------------------
    !! Returns true if side is top side (y-coordinate >= 0)
    !! Based on convention that airfoil is normalized with LE at (0,0),
    !! and second point determines which side: y(2) >= 0 for top, y(2) < 0 for bottom
    !-----------------------------------------------------------------------------

    type(side_airfoil_type), intent(in) :: side
    logical :: top_side

    top_side = (side%y(2) >= 0d0)

  end function is_top



  pure elemental function is_bot(side) result(bot_side)

    !-----------------------------------------------------------------------------
    !! Returns true if side is bottom side (y-coordinate < 0)
    !! Based on convention that airfoil is normalized with LE at (0,0),
    !! and second point determines which side: y(2) >= 0 for top, y(2) < 0 for bottom
    !-----------------------------------------------------------------------------

    type(side_airfoil_type), intent(in) :: side
    logical :: bot_side

    bot_side = (side%y(2) < 0d0)

  end function is_bot

end module