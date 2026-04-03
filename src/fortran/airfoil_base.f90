! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2025 Jochen Guenzel

module airfoil_base

  ! Airfoil type and basic operations 

  use os_util 
  use commons
  use print_util
  use string_util,        only : stri

  use spline,             only : spline_2D_type, spline_2D
  use shape_bezier,       only : bezier_spec_type, bezier_eval_side  
  use shape_bspline,      only : bspline_spec_type, bspline_eval_side, write_bspline_file
  use shape_hicks_henne,  only : hh_spec_type

  implicit none
  private

  double precision, parameter   :: EPSILON = 1.d-10           ! numerical accuracy of geo 

  ! --- main airfoil type ------------------------------------------------------------

  ! Single side of airfoil 

  type side_airfoil_type 
    character(:), allocatable     :: name             ! either 'Top' or 'Bot' or 'thickness' ...
    double precision, allocatable :: x(:)
    double precision, allocatable :: y(:)
    double precision, allocatable :: curvature(:)
  end type 

  ! the airfoil type

  type airfoil_type 

    character(:), allocatable     :: name               ! name of the airfoil
    double precision, allocatable :: x(:)               ! airfoil coordinates
    double precision, allocatable :: y(:)
    logical :: symmetrical    = .false.                  ! airfoil symmetrical? -> bot equals top side

    type (side_airfoil_type)      :: top                 ! top side of airfoil
    type (side_airfoil_type)      :: bot                 ! bottom side of airfoil 

    type (spline_2D_type)         :: spl                 ! cubic spline of coordinates 

    type (bezier_spec_type)       :: top_bezier          ! bezier curve specification if 'bezier_based'
    type (bezier_spec_type)       :: bot_bezier          ! bezier curve specification if 'bezier_based'

    type (bspline_spec_type)       :: top_bspline        ! bspline curve specification if 'bspline_based'
    type (bspline_spec_type)       :: bot_bspline        ! bspline curve specification if 'bspline_based'

    character (:), allocatable    :: hh_seed_name        ! name of seed airfoil hh are applied on 
    double precision, allocatable :: hh_seed_x(:)        ! seed coordinates for rebuild (write)  
    double precision, allocatable :: hh_seed_y(:)              
    type (hh_spec_type)           :: top_hh              ! hh specs for top side 
    type (hh_spec_type)           :: bot_hh              ! hh specs for bot side 
 
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

  ! --- public functions ------------------------------------------------------------

  public :: airfoil_load
  public :: airfoil_load_bezier
  public :: airfoil_from_bezier, airfoil_from_bspline, airfoil_from_spline
  public :: airfoil_write, airfoil_write_with_shapes
  public :: is_dat_file
  public :: is_bezier_based, is_bspline_based, is_hh_based, is_dat_based, set_as_dat_based
  public :: print_airfoil_write
  public :: build_from_sides
  public :: split_foil_into_sides 
  public :: is_normalized_coord
  public :: make_symmetrical
  public :: airfoil_name_flapped
  public :: create_bezier_example_airfoil, create_bezier_MH30, create_bezier_JX_GS3_100
  public :: le_find_xy_of_spline
  public :: normalize, te_gap

contains

  subroutine airfoil_load (filename, foil)

    !----------------------------------------------------------------------------
    !! Reads an airfoil from a file (checks ordering)
    !----------------------------------------------------------------------------

    character(*), intent(in) :: filename
    type(airfoil_type), intent(out) :: foil

    logical :: labeled
    integer :: i, np
    double precision, allocatable   :: xtemp(:), ytemp(:)

    if (trim(filename) == '') then
      call my_stop ('No airfoil file defined either in input file nor as command line argument')
    end if 

    ! Read number of points and allocate coordinates

    call airfoil_points(filename, np, labeled)

    allocate(foil%x(np))
    allocate(foil%y(np))

    ! Read airfoil from file

    call airfoil_read(filename, np, labeled, foil%name, foil%x, foil%y)

    ! Change point ordering to counterclockwise, if necessary

    if (foil%y(np) > foil%y(1)) then
      
      call print_warning ('Changing point ordering to counter-clockwise ...')
      
      xtemp = foil%x
      ytemp = foil%y
      do i = 1, np
        foil%x(i) = xtemp (np-i+1)
        foil%y(i) = ytemp (np-i+1)
      end do

    end if

  end subroutine



  function airfoil_load_bezier (filename, npoint) result(foil)

    !----------------------------------------------------------------------------
    !! Read a Bezier airfoil from a .bez file and return a fully populated foil:
    !!   - foil%x, foil%y       - airfoil coordinates (npoint)
    !!   - foil%top_bezier      - top control points
    !!   - foil%bot_bezier      - bottom control points
    !!   - foil%top, foil%bot   - side_airfoil_type with curvature
    !----------------------------------------------------------------------------

    use shape_bezier,   only : read_bezier_file

    character(*), intent(in) :: filename
    integer, intent(in) :: npoint
    type(airfoil_type) :: foil

    call read_bezier_file (filename, 'Top', foil%name, foil%top_bezier)
    call read_bezier_file (filename, 'Bot', foil%name, foil%bot_bezier)
    foil = airfoil_from_bezier (foil%top_bezier, foil%bot_bezier, npoint, foil%name)

  end function



  function airfoil_from_bezier (top_bezier, bot_bezier, npoint, name) result(foil)

    !----------------------------------------------------------------------------
    !! Build complete airfoil from bezier curve specifications
    !! This centralizes the common pattern:  !!   - calculate npoint_top and npoint_bot from npoint
    !!   - evaluate both bezier sides (including analytical curvature)
    !!   - create side_airfoil_type for top and bot
    !!   - combine into full airfoil
    !!   - set bezier specs and flags
    !----------------------------------------------------------------------------

    use spline, only : spline_2D

    type(bezier_spec_type), intent(in)  :: top_bezier, bot_bezier
    integer, intent(in)                 :: npoint
    character(*), intent(in), optional  :: name
    type(airfoil_type) :: foil

    type(side_airfoil_type) :: top_side, bot_side
    integer :: npoint_top, npoint_bot

    ! Calculate point distribution
    npoint_bot = (npoint + 1) / 2
    npoint_top = (npoint + 1) - npoint_bot

    ! Build top side with analytical curvature
    top_side%name = 'Top'
    call bezier_eval_side (top_bezier, npoint_top, top_side%x, top_side%y, top_side%curvature)

    ! Build bot side with analytical curvature
    bot_side%name = 'Bot'
    call bezier_eval_side (bot_bezier, npoint_bot, bot_side%x, bot_side%y, bot_side%curvature)

    ! Combine sides into full airfoil
    foil = combine_airfoil_sides (top_side, bot_side)

    ! Build 2D spline for complete airfoil
    foil%spl = spline_2D (foil%x, foil%y)

    ! Set name if provided
    if (present(name)) then
      foil%name = name
    end if

    ! Set bezier specifications and flag
    foil%top_bezier = top_bezier
    foil%bot_bezier = bot_bezier

  end function



  function airfoil_load_bspline (filename, npoint) result(foil)

    !----------------------------------------------------------------------------
    !! Read a BSpline airfoil from a .bsp file and return a fully populated foil:
    !!   - foil%x, foil%y       - airfoil coordinates (npoint)
    !!   - foil%top_bspline      - top control points
    !!   - foil%bot_bspline      - bottom control points
    !!   - foil%top, foil%bot   - side_airfoil_type with curvature
    !----------------------------------------------------------------------------

    use shape_bspline,   only : read_bspline_file

    character(*), intent(in) :: filename
    integer, intent(in) :: npoint
    type(airfoil_type) :: foil

    call read_bspline_file (filename, 'Top', foil%name, foil%top_bspline)
    call read_bspline_file (filename, 'Bot', foil%name, foil%bot_bspline)
    foil = airfoil_from_bspline (foil%top_bspline, foil%bot_bspline, npoint, foil%name)

  end function



  function airfoil_from_bspline (top_bspline, bot_bspline, npoint, name) result(foil)

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
    character(*), intent(in), optional  :: name
    type(airfoil_type) :: foil

    type(side_airfoil_type) :: top_side, bot_side
    integer :: npoint_top, npoint_bot

    ! Calculate point distribution
    npoint_bot = (npoint + 1) / 2
    npoint_top = (npoint + 1) - npoint_bot

    ! Build top side with analytical curvature
    top_side%name = 'Top'
    call bspline_eval_side (top_bspline, npoint_top, top_side%x, top_side%y, top_side%curvature)

    ! Build bot side with analytical curvature
    bot_side%name = 'Bot'
    call bspline_eval_side (bot_bspline, npoint_bot, bot_side%x, bot_side%y, bot_side%curvature)

    ! Combine sides into full airfoil
    foil = combine_airfoil_sides (top_side, bot_side)

    ! Build 2D spline for complete airfoil
    foil%spl = spline_2D (foil%x, foil%y)

    ! Set name if provided
    if (present(name)) then
      foil%name = name
    end if

    ! Set bspline specifications and flag
    foil%top_bspline = top_bspline
    foil%bot_bspline = bot_bspline

  end function



  function airfoil_from_spline (foil_in, panel_options, name) result(foil)

    !----------------------------------------------------------------------------
    !! Build complete airfoil from spline specification with repaneling
    !! Consistent with airfoil_from_bezier/bspline pattern:
    !!   - takes spline curve specification as input
    !!   - applies cosine distribution based on panel_options
    !!   - evaluates at new points
    !!   - returns new airfoil with updated spline
    !----------------------------------------------------------------------------

    use spline,     only : eval_spline, spline_2D
    use math_util,  only : cosine_distribution

    type(airfoil_type), intent(in)        :: foil_in
    type(panel_options_type), intent(in)  :: panel_options
    character(*), intent(in), optional    :: name

    type(spline_2D_type)            :: spl_in
    type(airfoil_type)              :: foil
    integer                         :: npoint, nPanels, nPan_top, nPan_bot 
    double precision                :: s_start, s_end, s_le, xTe, yTe
    double precision, allocatable   :: u_top(:), u_bot(:), s(:), s_top(:), s_bot(:)

    ! extrakt te for le_find and spline
    xTe = (foil_in%x(1) + foil_in%x(size(foil_in%x))) / 2d0
    yTe = (foil_in%y(1) + foil_in%y(size(foil_in%y))) / 2d0

    spl_in = foil_in%spl

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
    s_le    = le_find_s_of_spline (spl_in, xTe, yTe) 
    s_end   = spl_in%s(size(spl_in%s))

    ! normalized point distribution u 

    u_top = cosine_distribution (nPan_top+1, panel_options%le_bunch, panel_options%te_bunch)
    u_top = u_top (size(u_top) : 1 : -1)        ! flip
    s_top = s_start + abs (u_top - 1d0) * s_le

    u_bot = cosine_distribution (nPan_bot+1, panel_options%le_bunch, panel_options%te_bunch)
    s_bot = s_le + u_bot * (s_end - s_le) 

    ! add new top and bot distributions 

    s = [s_top, s_bot(2:)]  

    ! Allocate arrays for new coordinates
    allocate(foil%x(npoint))
    allocate(foil%y(npoint))

    ! new calculated x,y coordinates  

    call eval_spline (spl_in, s, foil%x, foil%y) 

    ! Re-spline with new coordinates 

    foil%spl = spline_2D (foil%x, foil%y) 

    ! Normalize to 0..1 if not already normalized (important for repaneling and later operations)

    call normalize (foil)


    ! Set name if provided
    if (present(name)) then
      foil%name = name
    end if

  end function airfoil_from_spline



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



  subroutine airfoil_read (filename, npoints, labeled, name, x, y)

    !! read an airfoil. Assumes the number of points is already known.
    !! Also checks for incorrect format.

    character(*), intent(in)                :: filename
    character(:), allocatable, intent(out)  :: name
    integer, intent(in)                     :: npoints
    logical, intent(in) :: labeled
    double precision, intent(inout) :: x (:), y(:)

    integer :: i, iunit, ioerr, nswitch
    double precision :: dir1, dir2

    ! Open airfoil file

    iunit = 12
    open(unit=iunit, file=filename, status='old', position='rewind', iostat=ioerr)
    if (ioerr /= 0) then
      call my_stop ('Cannot find airfoil file '//trim(filename))
    end if

    ! Read points from file

    name = repeat(' ',250)
    if (labeled) read(iunit,'(A)') name
    name = trim(adjustl(name))

    do i = 1, npoints

      read(iunit,*,end=500,err=500) x(i), y(i)

      x(i) = x(i) + 0d0                             ! get rid of -0d0
      y(i) = y(i) + 0d0 
    end do

    close(iunit)

    ! Check that coordinates are formatted  

    nswitch = 0
    dir1 = x(2) - x(1)
    do i = 3, npoints
      dir2 = x(i) - x(i-1)
      if (dir2 /= 0.d0) then
        if (dir2*dir1 < 0.d0) nswitch = nswitch + 1
        dir1 = dir2
      end if
    end do

    if (nswitch /= 1) then
    ! Open the file again only to avoid error at label 500.
      open(unit=iunit, file=filename, status='old')
    else
      return
    end if

    500 close(iunit)

    call my_stop ("Incorrect .dat file format at line "//stri(i))

  end subroutine airfoil_read



  function is_dat_file (filename)

    !! .true. if filename has ending '.dat'

    character(*),  intent(in) :: filename
    logical                   :: is_dat_file 
    character(:), allocatable :: suffix 
    
    suffix = filename_suffix (filename)
    is_dat_file = suffix == '.dat' .or. suffix =='.DAT'

  end function  


  function is_bezier_based (foil) result(is_bez)

    !! is airfoil based on Bezier curves? Checks if bezier control points are allocated.

    type(airfoil_type), intent(in) :: foil
    logical :: is_bez

    is_bez = allocated(foil%top_bezier%px) .and. allocated(foil%bot_bezier%px)

  end function is_bezier_based


  function is_bspline_based (foil) result(is_bsp)

    !! is airfoil based on BSpline curves? Checks if bspline control points are allocated.

    type(airfoil_type), intent(in) :: foil
    logical :: is_bsp

    is_bsp = allocated(foil%top_bspline%px) .and. allocated(foil%bot_bspline%px)

  end function is_bspline_based


  function is_hh_based (foil) result(is_hh)

    !! is airfoil based on Hicks Henne functions? Checks if hh specs are allocated.

    type(airfoil_type), intent(in) :: foil
    logical :: is_hh

    is_hh = allocated(foil%top_hh%hhs) .and. allocated(foil%bot_hh%hhs)

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
      deallocate(foil%top_bezier%px)
      deallocate(foil%bot_bezier%px)
    end if 

    if (is_bspline_based (foil)) then 
      deallocate(foil%top_bspline%px)
      deallocate(foil%bot_bspline%px)
    end if 

    if (is_hh_based (foil)) then 
      deallocate(foil%top_hh%hhs)
      deallocate(foil%bot_hh%hhs)
    end if 

    ! ensure spline is updated to match coordinates 
    foil%spl = spline_2D (foil%x, foil%y)

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
    integer ile

    if (.not. is_normalized_coord (foil)) then 
        call my_stop ("split_foil: LE isn't at 0,0 or TE isn't symmetrical at x=1")
    end if  

    !! build 2D spline 

    foil%spl = spline_2D (foil%x, foil%y)

    ! get curvature of complete surface

    curv = eval_spline_curvature (foil%spl, foil%spl%s) 
    
    ! split the polylines

    ile = minloc (foil%x, 1)

    foil%top%name = 'Top'
    foil%top%x = foil%x(iLe:1:-1)
    foil%top%y = foil%y(iLe:1:-1)
    foil%top%curvature = curv(iLe:1:-1)

    foil%bot%name = 'Bot'

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
    np      = npt + npb - 1

    if (allocated(foil%x)) deallocate(foil%x)
    if (allocated(foil%y)) deallocate(foil%y)
    allocate(foil%x(np))
    allocate(foil%y(np))

    foil%x(1:npt) = foil%top%x (npt:1:-1)
    foil%y(1:npt) = foil%top%y (npt:1:-1)

    foil%x(npt:)  = foil%bot%x 
    foil%y(npt:)  = foil%bot%y  

    foil%top%name = 'Top'
    foil%bot%name = 'Bot'
   
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
      foil%bot_bezier%px =  foil%top_bezier%px 
      foil%bot_bezier%py = -foil%top_bezier%py
    else if (is_bspline_based (foil)) then
      foil%bot_bspline%px =  foil%top_bspline%px 
      foil%bot_bspline%py = -foil%top_bspline%py
    end if 

  end subroutine 



  subroutine airfoil_write (pathFileName, foil)
     
    !-----------------------------------------------------------------------------
    !! Writes an airfoil to a labeled file
    !-----------------------------------------------------------------------------
    
    character(*), intent(in)        :: pathFileName
    type(airfoil_type), intent(in)  :: foil

    integer                         :: iunit, ioerr, i
    character(len=512)              :: msg

    ! Open file for writing and out ...

    iunit = 13
    open  (unit=iunit, file=pathFileName, status='replace',  iostat=ioerr, iomsg=msg)
    if (ioerr /= 0) then 
      call my_stop ("Unable to write to file '"//trim(pathFileName)//"': "//trim(msg))
    end if 

    write(iunit,'(A)') trim(foil%name)

    ! Write coordinates

    do i = 1, size(foil%x)
      write(iunit,'(2F12.7)') foil%x(i), foil%y(i)
    end do

    close (iunit)

  end subroutine 



  subroutine print_airfoil_write (dir, fileName, file_type, highlight)
     
    !-----------------------------------------------------------------------------
    !! print user message about writing an airfoil 
    !! If 'highlight' the airfoil name will be highlighted
    !-----------------------------------------------------------------------------
    
    character(*), intent(in)        :: dir, fileName, file_type
    logical, intent(in), optional   :: highlight 

    logical                         :: do_highlight

    if (present(highlight)) then 
      do_highlight = highlight 
    else 
      do_highlight = .true. 
    end if 

    if (.not. show_details .and. .not. do_highlight) return 


    if (file_type == "bez") then 
      call print_action ("Writing bezier      ", no_crlf = .true.)
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

    character (:), allocatable      :: fileName 
    logical                         :: do_highlight

    if (present(highlight)) then 
      do_highlight = highlight 
    else 
      do_highlight = .true. 
    end if 

    ! write normal .dat 

    fileName = foil%name//'.dat'

    call print_airfoil_write (output_dir, fileName, 'dat', highlight=do_highlight)

    call airfoil_write (path_join (output_dir, fileName), foil)
    
    ! write bezier .bez 

    if (is_bezier_based (foil)) then

      fileName = foil%name//'.bez'
      call print_airfoil_write (output_dir, fileName, 'bez', highlight=do_highlight)

      call write_bezier_file (path_join (output_dir, fileName), foil%name, foil%top_bezier, foil%bot_bezier)
    
    ! write bspline .bsp 

    else if (is_bspline_based (foil)) then

      fileName = foil%name//'.bsp'
      call print_airfoil_write (output_dir, fileName, 'bsp', highlight=do_highlight)

      call write_bspline_file (path_join (output_dir, fileName), foil%name, foil%top_bspline, foil%bot_bspline)

    ! write hicks-henne .hicks 

    else if (is_hh_based (foil)) then

      fileName = foil%name//'.hicks'
      call print_airfoil_write (output_dir, fileName, 'hicks', highlight=do_highlight)
  
      call write_hh_file (path_join (output_dir, fileName), foil%name, foil%top_hh, foil%bot_hh, &
                          foil%hh_seed_name, foil%hh_seed_x, foil%hh_seed_y)

    end if 
  
  end subroutine 



  function airfoil_name_flapped (foil, angle, base_name) result (flapped_name) 
     
    !-----------------------------------------------------------------------------
    !! returns name of airfoil being flapped with angle (in degrees)
    !-----------------------------------------------------------------------------
    
    type(airfoil_type), intent(in)      :: foil
    double precision, intent(in)        :: angle
    character (*), intent(in), optional :: base_name 
    character(:), allocatable           :: flapped_name
    character (20)                      :: text_degrees

    if (present (base_name)) then 
      flapped_name = base_name
    else
      flapped_name = foil%name
    end if 

    if (angle /= 0) then 

      if (int(angle)*10  == int(angle*10d0)) then       !degree having decimal?
        write (text_degrees,'(SP,I3)') int (angle)
      else
        write (text_degrees,'(SP,F6.1)') angle
      end if
      flapped_name = flapped_name // '_f' // trim(adjustl(text_degrees))

    end if 

  end function 


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

    foil = airfoil_from_bezier (top_bezier, bot_bezier, npoint, "Bezier_Example")

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

    foil = airfoil_from_bezier (top_bezier, bot_bezier, npoint, "MH30-norm-bezier")

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

    foil = airfoil_from_bezier (top_bezier, bot_bezier, npoint, "JX-GS3-100_v6")

  end function



  function le_find_s_of_spline (spl, xTe, yTe) result (sle)

    !----------------------------------------------------------------------------
    !! Find leading edge arc length on a spline using Newton iteration
    !! LE is defined as the point where tangent is perpendicular to TE chord
    !----------------------------------------------------------------------------

    use math_util, only : clip
    use spline,    only : eval_spline

    type (spline_2D_type), intent(in) :: spl
    double precision, intent(in)      :: xTe, yTe

    double precision :: sle, x, y, dx, dy, ddx, ddy, dot, ddot
    double precision :: dxTe, dyTe, ds, s_start, s_end
    integer :: iter
    double precision, parameter :: EPS = 1d-10

    ! Get arc length bounds
    s_start = spl%s(1)
    s_end = spl%s(size(spl%s))

    ! Initial guess: midpoint of arc length
    sle = (s_start + s_end) / 2d0

    ! Newton iteration to find LE
    do iter = 1, 50

      sle = clip (sle, s_start + 0.1d0, s_end - 0.1d0)
      call eval_spline (spl, sle, x, y, 0)
      call eval_spline (spl, sle, dx, dy, 1)
      dxTe = x - xTe
      dyTe = y - yTe
      dot = dx * dxTe + dy * dyTe

      if ((abs(dot) < EPS)) exit

      call eval_spline (spl, sle, ddx, ddy, 2)
      ddot = dx**2 + dy**2 + dxTe * ddx + dyTe * ddy
      ds = - dot / ddot
      sle = sle + ds

    end do

    ! If didn't converge, return midpoint
    if (abs(dot) >= EPS) then
      sle = (s_start + s_end) / 2d0
    end if

  end function



  subroutine le_find_xy_of_spline (foil, xle, yle) 

    !----------------------------------------------------------------------------
    !! Find real leading edge coordinates from spline
    !! Returns x,y coordinates where tangent is perpendicular to TE chord
    !----------------------------------------------------------------------------

    use spline, only : eval_spline

    type (airfoil_type), intent(in)   :: foil 
    double precision, intent(out)     :: xle, yle

    double precision  :: sle, xTe, yTe
    integer :: n

    n = size(foil%x)
    xTe = (foil%x(1) + foil%x(n)) / 2d0
    yTe = (foil%y(1) + foil%y(n)) / 2d0
    print * , "xte, yte: ", xTe, yTe
    sle = le_find_s_of_spline (foil%spl, xTe, yTe)
    call eval_spline (foil%spl, sLe,  xle,  yle, 0)

  end subroutine



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

    double precision :: foilscale_upper, foilscale_lower
    double precision :: angle, cosa, sina
    double precision :: xle, yle, xi, yi, te_gap_old

    integer :: npoints, i, ile
    logical :: just_basic

    npoints = size(foil%x)
    te_gap_old = te_gap (foil)

    ! basic normalize or based on spline? 

    if (present(basic)) then 
      just_basic = basic 
    else 
      just_basic = .false.
    end if 

    if (just_basic) then 

      ile = minloc (foil%x, 1)
      xle = foil%x(ile) 
      yle = foil%y(ile) 

    else

      call le_find_xy_of_spline (foil, xle, yle)     ! get the 'real' leading edge of spline 

    end if 

    ! Translate so that the leading edge is at the origin

    do i = 1, npoints
      foil%x(i) = foil%x(i) - xle
      foil%y(i) = foil%y(i) - yle
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

  end subroutine normalize

end module