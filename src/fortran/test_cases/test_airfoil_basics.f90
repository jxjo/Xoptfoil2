! MIT License
! Copyright (c) 2024 Jochen Guenzel

module test_airfoil_basics
  
  !-------------------------------------------------------------------------
  ! Airfoil basic functions like split ...
  !-------------------------------------------------------------------------

  use os_util
  use test_util
  use airfoil_base,         only : airfoil_type, panel_options_type 
  use airfoil_base,         only : split_foil_into_sides, rebuild_from_sides
  use shape_bezier,         only : bezier_spec_type
  use shape_bezier,         only : create_bezier_example_airfoil,create_bezier_MH30     

  implicit none

  contains

  subroutine test_airfoil_split  ()

    !! test of split airfoil into top and bot 

    character (:), allocatable      :: name 
    double precision, allocatable   :: x(:), y(:)
    type(airfoil_type)              :: airfoil, new_airfoil 
    type(bezier_spec_type)          :: bezier, bot_bezier 
    ! integer :: i

    call test_header ("Airfoil split")

    call create_bezier_example_airfoil (201, name, x, y, bezier, bot_bezier)

    airfoil%x = x
    airfoil%y = y
    airfoil%name = name
    airfoil%symmetrical = .false. 

    call split_foil_into_sides (airfoil) 

    call asserti (size(airfoil%bot%x), 101, "Number of x points after split")
    call asserti (size(airfoil%bot%y), 101, "Number of y points after split")

    call assertf (sum(airfoil%top%y), 4.448841d0, "Checksum top y coordinates", 6)

    ! print *, "maxval ", maxval(airfoil%top%curvature), maxval(airfoil%bot%curvature)
    ! print *, "minval ", minval(airfoil%top%curvature), minval(airfoil%bot%curvature)

    call assertf (minval(airfoil%top%curvature), 0.17d0, "Min top curvature", 2)
    call assertf (minval(airfoil%bot%curvature), 0.06d0, "Min bot curvature", 2)
    call assertf (maxval(airfoil%top%curvature), 78.5d0, "Max top curvature", 1)
    call assertf (maxval(airfoil%bot%curvature),111.7d0, "Max bot curvature", 1)

    ! write(*,"('top curv LE: ',200f7.1)") ( airfoil%top%curvature(i), i=1,10 )
    ! write(*,"('bot curv LE: ',200f7.1)") ( airfoil%bot%curvature(i), i=1,10 )

    ! write(*,"('top curv TE: ',200f6.2)") ( airfoil%top%curvature(i), i=92,101 )
    ! write(*,"('bot curv TE: ',200f6.2)") ( airfoil%bot%curvature(i), i=92,101)

    ! --- now rebuild airfoil again 

    new_airfoil = airfoil 

    call rebuild_from_sides (new_airfoil)
    call assertf (sum(airfoil%x), sum(new_airfoil%x), "Rebuilded airfoil x coordinates ")
    call assertf (sum(airfoil%y), sum(new_airfoil%y), "Rebuilded airfoil z coordinates ")


  end subroutine


  
  subroutine test_airfoil_normalize  ()

    !! test of split airfoil into top and bot 

    use airfoil_geometry,     only : repanel_and_normalize, le_find

    character (:), allocatable      :: name 
    double precision, allocatable   :: x(:), y(:)
    type(airfoil_type)              :: airfoil, new_airfoil 
    type(bezier_spec_type)          :: bezier, bot_bezier 
    double precision                :: xle, yle
    ! integer :: i

    call test_header ("Airfoil normalize")

    call create_bezier_example_airfoil (201, name, x, y, bezier, bot_bezier)

    airfoil%x = x
    airfoil%y = y
    airfoil%name = name
    airfoil%symmetrical = .false. 
    call split_foil_into_sides (airfoil) 

    call assertf (airfoil%top%curvature(1), 78.5d0, "le top curvature before ", 1)

    call repanel_and_normalize (airfoil, new_airfoil) 

    call asserti (size(new_airfoil%x), 161, "No of points for curvature results")

    call assertf (new_airfoil%top%curvature(1), 77.0d0, "le top curvature after  ", 1)

    call le_find (new_airfoil, xle, yle)
    call assertf (xle, 0d0, "le x = 0.0 ", 7)
    call assertf (yle, 0d0, "le y = 0.0 ", 7)

  end subroutine



  subroutine test_airfoil_geometry  ()

    !! test of geometry info like thickness 

    use airfoil_geometry,   only : get_geometry, repanel_and_normalize, set_geometry
    use airfoil_geometry,   only : set_geometry_by_scale

    character (:), allocatable      :: name 
    double precision, allocatable   :: x(:), y(:), top_x(:), bot_x(:)
    type(airfoil_type)              :: airfoil, new_airfoil 
    type(bezier_spec_type)          :: bezier, bot_bezier 
    double precision                :: t, xt, c, xc
 
    call test_header ("Airfoil geometry")

    call create_bezier_MH30 (201, name, x, y, bezier, bot_bezier)

    airfoil%x = x
    airfoil%y = y
    airfoil%name = name
    airfoil%symmetrical = .false. 

    call repanel_and_normalize (airfoil, new_airfoil)

    top_x = new_airfoil%top%x
    bot_x = new_airfoil%bot%x

    ! get geometry 

    call get_geometry (new_airfoil, t, xt, c, xc) 

    call asserti (size(new_airfoil%x), 161, "No of points for geometry results")
    call assertf (t,   7.8567d-2, "Max thickness      "//strf('(F7.4)', t*1d2)//"%", 6)
    call assertf (xt, 0.292803d0, "Max thickness pos "//strf('(F7.4)', xt*1d2)//"%", 6)
    call assertf (c,   1.7042d-2, "Max camber         "//strf('(F7.4)', c*1d2)//"%", 6)
    call assertf (xc, 0.454123d0, "Max camber pos    "//strf('(F7.4)', xc*1d2)//"%", 6)

    ! set geometry 

    call set_geometry (new_airfoil, maxt = 0.1d0)
    call set_geometry (new_airfoil, xmaxt= 0.2d0)
    call set_geometry (new_airfoil, maxc = 0.02d0)
    call set_geometry (new_airfoil, xmaxc= 0.4d0)

    call get_geometry (new_airfoil, t, xt, c, xc) 
    call assertf (t,   0.1d0, "Set thickness     "//strf('(F7.4)',  t*1d2)//"%", 6)
    call assertf (xt,  0.2d0, "Set thickness pos "//strf('(F7.4)', xt*1d2)//"%", 5)
    call assertf (c,  0.02d0, "Set camber         "//strf('(F7.4)',  c*1d2)//"%", 6)
    call assertf (xc,  0.4d0, "Set camber pos    "//strf('(F7.4)', xc*1d2)//"%", 5)

    ! scale geometry 

    call set_geometry_by_scale (new_airfoil, 2d0, 1d0, 1d0, 1d0, 1d0, 0.1d0) 
    call get_geometry (new_airfoil, t, xt, c, xc) 
    call assertf (t,   0.2d0, "Scale thickness     "//strf('(F7.4)',  t*1d2)//"%", 6)

    call set_geometry_by_scale (new_airfoil, 1d0, 1.25d0, 1d0, 1d0, 1d0, 0.1d0) 
    call get_geometry (new_airfoil, t, xt, c, xc) 
    call assertf (xt,  0.4d0, "Scale thickness pos "//strf('(F7.4)', xt*1d2)//"%", 5)


    call set_geometry_by_scale (new_airfoil, 1d0, 1d0, 2d0, 1d0, 1d0, 0.1d0) 
    call get_geometry (new_airfoil, t, xt, c, xc) 
    call assertf (c,  0.04d0, "Scale camber         "//strf('(F7.4)',  c*1d2)//"%", 6)

    call set_geometry_by_scale (new_airfoil, 1d0, 1d0, 1d0, 1.5d0, 1d0, 0.1d0) 
    call get_geometry (new_airfoil, t, xt, c, xc) 
    call assertf (xc,   0.69999d0, "Scale camber pos    "//strf('(F7.4)',  xc*1d2)//"%", 5)

    ! check x coordinates didn't change 

    call assertf (sum(new_airfoil%top%x),  sum(top_x), "Check top x didn't change", 6)
    call assertf (sum(new_airfoil%bot%x),  sum(bot_x), "Check bot x didn't change", 6)


  end subroutine


end module 

