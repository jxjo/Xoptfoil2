! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2024 Jochen Guenzel

module airfoil_operations

  ! Airfoil geometry related operations - also using xfoils geo routines 

  use os_util 
  use commons
  use print_util

  implicit none

  double precision, parameter    :: EPSILON = 1.d-10          ! distance xfoil LE to 0,0
  double precision, parameter    :: EPSILON_TE = 1.d-8        ! z-value of TE to be zero 
  double precision, parameter    :: LE_PANEL_FACTOR = 0.4     ! lenght LE panel / length prev panel

contains

  subroutine get_seed_airfoil (airfoil_filename, foil )

    !-----------------------------------------------------------------------------------
    !! loads either .dat or .bez file into 'foil' 
    !-----------------------------------------------------------------------------------

    use shape_bezier, only : load_bezier_airfoil

    character(*), intent(in)        :: airfoil_filename
    type(airfoil_type), intent(out) :: foil

    character (:), allocatable  :: extension
    integer                     :: istart

    ! evaluate filetype from filename extension 

    istart = len(airfoil_filename) - 3
    extension = airfoil_filename (istart : )

    if (extension /= '.dat' .or. extension /= '.DAT') then 

    ! Read seed airfoil from .dat file

      call print_action ('Reading airfoil file', show_details, airfoil_filename)

      call load_airfoil(airfoil_filename, foil)

    else if (extension /= '.bez' .or. extension /= '.BEZ') then

    ! Read seed bezier control points from .bez file and generate airfoil

      call print_action ('Reading Bezier file', show_details, airfoil_filename)

      foil%is_bezier_based = .true.
      foil%npoint = 201                           ! 201 points as default - will be repaneled anyway

      call load_bezier_airfoil (airfoil_filename, foil%npoint, foil%name, foil%x, foil%y, foil%top_bezier, foil%bot_bezier) 

      call split_foil_at_00_into_sides (foil)     ! upper and lower will be needed for input sanity
  
    else

      call my_stop ("Unknown file extension: "//extension)
    
    end if

  end subroutine get_seed_airfoil


  subroutine load_airfoil(filename, foil)

    !----------------------------------------------------------------------------
    !! Reads an airfoil from a file (checks ordering)
    !----------------------------------------------------------------------------

    character(*), intent(in) :: filename
    type(airfoil_type), intent(out) :: foil

    logical :: labeled
    integer :: i
    double precision, allocatable   :: xtemp(:), ytemp(:)

    if (trim(filename) == '') then
      call my_stop ('No airfoil file defined either in input file nor as command line argument')
    end if 

    ! Read number of points and allocate coordinates

    call airfoil_points(filename, foil%npoint, labeled)

    allocate(foil%x(foil%npoint))
    allocate(foil%y(foil%npoint))

    ! Read airfoil from file

    call airfoil_read(filename, foil%npoint, labeled, foil%name, foil%x, foil%y)

    ! Change point ordering to counterclockwise, if necessary

    if (foil%y(foil%npoint) > foil%y(1)) then
      
      call print_warning ('Changing point ordering to counter-clockwise ...')
      
      xtemp = foil%x
      ytemp = foil%y
      do i = 1, foil%npoint
        foil%x(i) = xtemp (foil%npoint-i+1)
        foil%y(i) = ytemp (foil%npoint-i+1)
      end do

    end if

  end subroutine load_airfoil



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



  subroutine airfoil_read(filename, npoints, labeled, name, x, z)

    !! read an airfoil. Assumes the number of points is already known.
    !! Also checks for incorrect format.

    character(*), intent(in)  :: filename
    character(:), allocatable, intent(out) :: name
    integer, intent(in) :: npoints
    logical, intent(in) :: labeled
    double precision, dimension(:), intent(inout) :: x, z

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
      read(iunit,*,end=500,err=500) x(i), z(i)
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
    write (*,*)
    write (*,*)
    write(*,'(A)') "Incorrect format in "//trim(filename)//". File should"
    write(*,'(A)') "have x and y coordinates in 2 columns to form a single loop,"
    write(*,'(A)') "and there should be no blank lines.  See the user guide for"
    write(*,'(A)') "more information."
    call my_stop ("Processing stopped")

  end subroutine airfoil_read



  function te_gap (foil)

    !! trailing edge gap of foil 

    type(airfoil_type), intent(in)  :: foil
    double precision :: te_gap
  
    te_gap = sqrt ((foil%x(1) - foil%x(size(foil%x)))**2 + &
                   (foil%y(1) - foil%y(size(foil%y)))**2)
  end function 
  


  subroutine le_find (foil, ile_close, is_le)

    !! find the point index which is closest to the real le (xfoil spline) 
    !! If this point is EPSILON to le, is_le is .true. 

    use math_deps,      only : norm_2
    use xfoil_driver,   only : xfoil_le_find

    type (airfoil_type), intent(in) :: foil
    integer, intent(out)  :: ile_close
    logical, intent(out)  :: is_le

    integer :: i, npt
    double precision, allocatable :: x(:), y(:) 
    double precision, dimension(2) :: r1, r2
    double precision :: dist1, dist2, dot
    double precision :: xle, yle

    ile_close = 0
    is_le = .false.

    x = foil%X
    y = foil%y 
    npt = size(x)

    ! Get leading edge location from Xfoil

    call xfoil_le_find (foil, xle, yle)

    ! Determine leading edge index and where to add a point

    npt = size(x,1)
    do i = 1, npt-1
      r1(1) = xle - x(i)
      r1(2) = yle - y(i)
      dist1 = norm_2(r1)
      if (dist1 /= 0.d0) r1 = r1/dist1

      r2(1) = xle - x(i+1)
      r2(2) = yle - y(i+1)
      dist2 = norm_2(r2)
      if (dist2 /= 0.d0) r2 = r2/dist2

      dot = dot_product(r1, r2)
      if (dist1 < EPSILON) then                               ! point is defacto at 0,0 
        ile_close = i
        is_le = .true.
        exit
      else if (dist2 < EPSILON) then                          ! point is defacto at 0,0 
        ile_close = i+1
        is_le = .true.
        exit
      else if (dot < 0.d0) then
        if (dist1 < dist2) then
          ile_close = i
        else
          ile_close = i+1
        end if
        exit
      end if
    end do

  end subroutine 




  function is_normalized_coord (foil) result(is_norm)

    !! Checks if foil is normalized - only looking at coordinates (no real LE check)
    !!  - Leading edge at 0,0 
    !!  - Trailing edge at 1,0 (upper and lower side may have a gap) 

    type(airfoil_type), intent(in)  :: foil
    logical       :: is_norm
    integer       :: x_min_at

    is_norm = .true. 

    ! Check TE 

    if (foil%x(1) /= 1d0 .or. foil%x(size(foil%x)) /= 1d0)    is_norm = .false.  
    if ((foil%y(1) + foil%y(size(foil%x))) /= 0d0)            is_norm = .false.

    ! Check LE 

    x_min_at = (minloc (foil%x,1))
    if (foil%x(x_min_at) /= 0d0)                              is_norm = .false.
    if (foil%y(x_min_at) /= 0d0)                              is_norm = .false.

  end function is_normalized_coord



  function is_normalized (foil, npan) result(is_norm)

    !! Checks if foil is normalized 
    !!  - Leading edge at 0,0 
    !!  - Trailing edge at 1,0 (upper and lower side may have a gap) 
    !!  - Number of panels equal npan (xfoil_geo_options) or npan + 1 (LE was added)

    type(airfoil_type), intent(in)  :: foil
    integer, intent(in)             :: npan

    logical               :: is_norm, is_le
    integer               :: le

    is_norm = is_normalized_coord (foil)
    if (.not. is_norm) return 

    ! Check LE - use xfoil to find the real, splined LE

    call le_find (foil, le, is_le)
    if (.not. is_le) is_norm = .false.

    ! Check npan 

    if ((size(foil%x) /= npan) .and. & 
        (size(foil%x) /= npan + 1)) is_norm = .false.  

  end function is_normalized



  subroutine repanel_and_normalize (in_foil, xfoil_geom_options, foil)

    !-----------------------------------------------------------------------------
    !! Repanel an airfoil with npoints and normalize it to get LE at 0,0 and
    !!    TE at 1.0 (upper and lower side may have a gap)  
    !-----------------------------------------------------------------------------

    use math_deps,    only : norm_2, norm2p
    use xfoil_driver, only : xfoil_repanel, xfoil_geom_options_type, xfoil_set_airfoil, xfoil_le_find

    type(airfoil_type), intent(in)    :: in_foil
    type(airfoil_type), intent(out)   :: foil
    type(xfoil_geom_options_type),intent(in)    :: xfoil_geom_options

    type(airfoil_type)  :: tmp_foil
    integer             :: i, n, ile_close
    logical             :: le_fixed, inserted, is_le
    double precision    :: xle, yle
    double precision, dimension(2) :: p_next, p, p_prev
    character (:), allocatable     :: text

    !
    ! For normalization xfoils LEFIND is used to calculate the (virtual) LE of
    !    the airfoil - then it's shifted, rotated, scaled to be normalized.
    !
    ! Bad thing: a subsequent xfoil LEFIND won't deliver LE at 0,0 but still with a little 
    !    offset. SO this is iterated until the offset is smaller than epsilon
    !

    tmp_foil%npoint = xfoil_geom_options%npan
    allocate (tmp_foil%x(tmp_foil%npoint))  
    allocate (tmp_foil%y(tmp_foil%npoint))   

    foil%name = in_foil%name 

    ! initial paneling to npoint_new
    call xfoil_repanel(in_foil, foil, xfoil_geom_options)

    le_fixed = .false. 
    inserted = .false.
  
    do i = 1,10

      call normalize (foil)

      call xfoil_le_find (foil, xle, yle)

      if (norm2p (xle, yle)  < EPSILON) then
        le_fixed = .true. 
        exit 
      end if
      
      tmp_foil%x = foil%x
      tmp_foil%y = foil%y
      tmp_foil%name = foil%name 

      call xfoil_repanel(tmp_foil, foil, xfoil_geom_options)
      
    end do

    ! reached a virtual LE which is closer to 0,0 than epsilon, set it to 0,0

    if (le_fixed) then 

      call le_find (foil, ile_close, is_le)

      if (.not. is_le) then 
        ! is the LE panel of closest point much! shorter than the next panel? 
        !       if yes, take this point to LE 0,0
        p(1)      = foil%x(ile_close)
        p(2)      = foil%y(ile_close)
        p_next(1) = foil%x(ile_close + 1) - foil%x(ile_close)
        p_next(2) = foil%y(ile_close + 1) - foil%y(ile_close)
        p_prev(1) = foil%x(ile_close - 1) - foil%x(ile_close)
        p_prev(2) = foil%y(ile_close - 1) - foil%y(ile_close)
        if (((norm_2(p) / norm_2(p_next)) < LE_PANEL_FACTOR) .and. & 
            ((norm_2(p) / norm_2(p_prev)) < LE_PANEL_FACTOR)) then
          foil%x(ile_close) = 0d0
          foil%y(ile_close) = 0d0
        else

          ! add a new leading edge point at 0,0  
          call insert_point_at_00 (foil, inserted)

        end if 
      else

        ! point is already EPSILON at 0,0 - ensure 0,0 
        foil%x(ile_close) = 0d0                         
        foil%y(ile_close) = 0d0
      end if 
    else
      call print_warning ("Leading edge couln't be moved close to 0,0. Continuing ...",3)
      write (*,*)
    end if 

    ! te could be non zero due to numerical issues 

    n = size(foil%y)
    if (abs(foil%y(1)) < EPSILON_TE) foil%y(1) = 0d0 
    if (abs(foil%y(n)) < EPSILON_TE) foil%y(n) = 0d0 

    if ((foil%y(1) + foil%y(n)) < EPSILON_TE) foil%y(n) = - foil%y(1)     ! make te gap symmetrical

    ! now split airfoil to get upper and lower sides for future needs  

    call split_foil_at_00_into_sides (foil)

    foil%name = in_foil%name // '-norm'

    text = 'Repaneling and normalizing.'
    if (inserted) text = text //' Added leading edge point.'
    text = text // ' Airfoil will have '
    call print_action (text, show_details, stri(foil%npoint) //' Points') 

  end subroutine repanel_and_normalize



  subroutine normalize (foil)

    !-----------------------------------------------------------------------------
    !! Translates and scales an airfoil such that it has a 
    !! - length of 1 
    !! - leading edge of spline is at 0,0 and trailing edge is symmetric at 1,x
    !-----------------------------------------------------------------------------

    use xfoil_driver,   only : xfoil_le_find

    type(airfoil_type), intent(inout) :: foil

    double precision :: foilscale_upper, foilscale_lower
    double precision :: angle, cosa, sina
    double precision :: xle, yle

    integer :: npoints, i, ile

    npoints = foil%npoint

    ! get the 'real' leading edge of spline from xfoil 

    call xfoil_le_find (foil, xle, yle) 

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
      foil%x(i) = foil%x(i) * cosa - foil%y(i) * sina
      foil%y(i) = foil%x(i) * sina + foil%y(i) * cosa
    end do

    ! Ensure TE is at x=1

    If (foil%x(1) /= 1d0 .or. foil%x(npoints) /= 1d0) then 

      ! Scale airfoil so that it has a length of 1 
      ! - there are mal formed airfoils with different TE on upper and lower
      !   scale both to 1.0  

      ile = minloc (foil%x, 1)

      If (foil%x(1) /= 1d0) then 

        foilscale_upper = 1.d0 / foil%x(1)
        do i = 1, ile - 1
          foil%x(i) = foil%x(i)*foilscale_upper
          foil%y(i) = foil%y(i)*foilscale_upper
        end do

      else
        
        foilscale_lower = 1.d0 / foil%x(npoints)
        do i = ile + 1, npoints
            foil%x(i) = foil%x(i)*foilscale_lower
            foil%y(i) = foil%y(i)*foilscale_lower
        end do
      end if 

      foil%x(1)       = 1d0
      foil%x(npoints) = 1d0

    end if 

    ! Force TE to 0.0 if y < epsilon 

    if (foil%y(1) == foil%y(npoints) .and. abs(foil%y(1)) < EPSILON) then 
      foil%y(1) = 0.d0
      foil%y(npoints) = 0.d0 
    end if 

  end subroutine normalize



  subroutine repanel_bezier (foil_in, npoint, foil)

    !-----------------------------------------------------------------------------
    !! repanels a bezier based airfoil to npoint
    !-----------------------------------------------------------------------------

    use shape_bezier,   only : bezier_eval_airfoil

    type(airfoil_type), intent(in)  :: foil_in
    type(airfoil_type), intent(out) :: foil
    integer, intent(in)             :: npoint

    foil = foil_in
    call bezier_eval_airfoil (foil%top_bezier, foil%bot_bezier, npoint, foil%x, foil%y)
    call split_foil_at_00_into_sides (foil)

  end subroutine 




  subroutine insert_point_at_00 (foil, inserted)

    !! insert a new point at 0,0 being new leading edge  

    type(airfoil_type), intent(inout) :: foil
    logical, intent(out)              :: inserted
    double precision, allocatable     :: x_new(:), y_new(:)
    integer       :: i, j, npt 

    inserted = .false. 
    npt = size(foil%x) 

    allocate (x_new(npt+1))
    allocate (y_new(npt+1))

    j = 1

    do i = 1, npt
      if (foil%x(i) == 0d0 .and. foil%y(i) == 0d0) then     ! sanity check - there is already 0,0 
        return 
      else if (foil%y(i) > 0d0 .or. i < int(npt/4)) then 
        x_new(j) = foil%x(i)
        y_new(j) = foil%y(i)
        j = j + 1
      else
        x_new(j) = 0d0                                      ! insert new point at 0,0 
        y_new(j) = 0d0
        j = j + 1
        x_new(j) = foil%x(i)
        y_new(j) = foil%y(i)
        exit 
      end if 
    end do 

    ! copy the rest (bottom side)
    x_new(j+1:npt+1) = foil%x(i+1:npt)
    y_new(j+1:npt+1) = foil%y(i+1:npt)

    foil%x = x_new
    foil%y = y_new
    foil%npoint = size(foil%x)

    inserted = .true.

  end subroutine 



  subroutine split_foil_at_00_into_sides (foil)

    !-----------------------------------------------------------------------------
    !! Split an airfoil into its top and bottom side
    !! if there is already a leading edge at 0,0 
    !-----------------------------------------------------------------------------

    use xfoil_driver, only : xfoil_get_curvature

    type(airfoil_type), intent(inout) :: foil
    double precision, allocatable     :: curv (:) 
    integer ile

    ile = minloc (foil%x, 1)
    if (ile == 0 .or. foil%x(ile) /= 0d0 .or. foil%y(ile) /= 0d0) then 
      call my_stop ("Split_foil: Leading edge isn't at 0,0")
    end if  

    ! get curvature of complete surface
    !  -> use xfoils CURV based on spline (math_deps/curvature evals too low values at LE!) 

    curv = xfoil_get_curvature (foil)

    ! split the polylines

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



  subroutine rebuild_from_sides (top_side, bot_side, foil, name)

    !-----------------------------------------------------------------------------
    !! rebuild foil from a top and bot side - recalc curvature of top and bot 
    !! A new airfoil name can be set optionally 
    !-----------------------------------------------------------------------------

    use xfoil_driver, only : xfoil_get_curvature

    type(side_airfoil_type), intent(in)   :: top_side, bot_side
    type(airfoil_type), intent(inout)     :: foil
    character (*), optional, intent(in)   :: name

    double precision, allocatable         :: curv (:) 
    integer   :: pointst, pointsb

    pointst = size(top_side%x)
    pointsb = size(bot_side%x)
    foil%npoint = pointst + pointsb - 1

    if (allocated(foil%x)) deallocate(foil%x)
    if (allocated(foil%y)) deallocate(foil%y)
    allocate(foil%x(foil%npoint))
    allocate(foil%y(foil%npoint))

    foil%x(1:pointst) = top_side%x (pointst:1:-1)
    foil%y(1:pointst) = top_side%y (pointst:1:-1)

    foil%x(pointst:)  = bot_side%x 
    foil%y(pointst:)  = bot_side%y  

    foil%top  = top_side
    foil%top%name = 'Top'

    foil%bot  = bot_side  
    foil%bot%name = 'Bot'

    if (present(name)) then 
      foil%name = name 
    end if 
    
    ! get curvature of complete surface
    !  -> use xfoils CURV based on spline (math_deps/curvature evals too low values at LE!) 
    curv = xfoil_get_curvature (foil)

    foil%top%curvature = curv(pointst:1:-1)
    foil%bot%curvature = curv(pointst:)

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

    call rebuild_from_sides (foil%top, foil%bot, foil)

    if (foil%is_bezier_based) then
      foil%bot_bezier%px =  foil%top_bezier%px 
      foil%bot_bezier%py = -foil%top_bezier%py
    end if 

  end subroutine 



  subroutine airfoil_write(filename, name, foil)
     
    !! Writes an airfoil to a labeled file
    
    character(*), intent(in) :: filename, name
    type(airfoil_type), intent(in) :: foil
    integer :: iunit, ioerr
    character(len=512) :: msg

    ! sanity check 

    if (foil%npoint /= size(foil%x)) then 
      call my_stop ("Airfoil: Wrong format to write (npoint="//stri(foil%npoint)//").")
    end if 
    
    ! Open file for writing and out ...

    iunit = 13
    open  (unit=iunit, file=filename, status='replace',  iostat=ioerr, iomsg=msg)
    if (ioerr /= 0) then 
      call my_stop ("Unable to write to file '"//trim(filename)//"': "//trim(msg))
    end if 

    call print_action ("Writing airfoil to", show_details, filename)

    call airfoil_write_to_unit (iunit, name, foil)
    close (iunit)

  end subroutine airfoil_write


  subroutine airfoil_write_with_shapes (foil)

    !-----------------------------------------------------------------------------
    !! write airfoil .dat and bezier or hicks henne files 
    !-----------------------------------------------------------------------------

    use commons,            only : airfoil_type
    use shape_bezier,       only : write_bezier_file
    use shape_hicks_henne,  only : write_hh_file
 
    type (airfoil_type), intent(in) :: foil 

    character (:), allocatable      :: output_file 

    output_file = foil%name//'.dat'
    call airfoil_write (output_file, foil%name, foil)
  
    if (foil%is_bezier_based) then
      output_file = foil%name//'.bez'

      call print_action ('Writing Bezier to', show_details, output_file)

      call write_bezier_file (output_file, foil%name, foil%top_bezier, foil%bot_bezier)
  
    else if (foil%is_hh_based) then
      output_file = foil%name//'.hicks'

      call print_action ('Writing Hicks-Henne to', show_details, output_file)

      call write_hh_file (output_file, foil%hh_seed_name, foil%top_hh, foil%bot_hh)

    end if 
  
  end subroutine 



  subroutine airfoil_write_to_unit (iunit, title, foil)

    !! Writes an airfoil with a title to iunit
    !    --> central function for all foil coordinate writes

    integer, intent(in)             :: iunit
    character(*), intent(in)        :: title
    type(airfoil_type), intent(in)  :: foil
    integer :: i

    ! Write label to file
    
    write(iunit,'(A)') trim(title)

    ! Write coordinates

    do i = 1, foil%npoint
      write(iunit,'(2F12.7)')         foil%x(i), foil%y(i)
    end do

  end subroutine airfoil_write_to_unit

end module airfoil_operations