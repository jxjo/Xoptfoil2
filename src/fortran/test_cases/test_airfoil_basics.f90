! MIT License

module test_airfoil_basics
  
  !-------------------------------------------------------------------------
  ! Airfoil basic functions like split ...
  !-------------------------------------------------------------------------

  use os_util
  use test_util
  use commons,              only : TOP, BOT
  use airfoil_base,         only : airfoil_type, panel_options_type, repanel
  use airfoil_base,         only : split_foil_into_sides, build_from_sides
  use airfoil_base,         only : create_bezier_example_airfoil, create_bezier_MH30
  use airfoil_base,         only : airfoil_write_dat, airfoil_load_dat
  use shape_bezier,         only : bezier_spec_type     

  implicit none

  contains

  subroutine test_airfoil_split  ()

    !! test of split airfoil into top and bot 

    type(airfoil_type)              :: airfoil, new_airfoil 

    call test_header ("Airfoil split")

    airfoil = create_bezier_example_airfoil (201)
    call split_foil_into_sides (airfoil) 

    call asserti (size(airfoil%bot%x), 101, "Number of x points after split")
    call asserti (size(airfoil%bot%y), 101, "Number of y points after split")

    call assertf (sum(airfoil%top%y), 4.262326d0, "Checksum top y coordinates", 6)

    ! print *, "maxval ", maxval(airfoil%top%curvature), maxval(airfoil%bot%curvature)
    ! print *, "minval ", minval(airfoil%top%curvature), minval(airfoil%bot%curvature)

    call assertf (minval(airfoil%top%curvature), 0.17d0, "Min top curvature", 2)
    call assertf (minval(airfoil%bot%curvature), 0.06d0, "Min bot curvature", 2)
    call assertf (maxval(airfoil%top%curvature), 82.7d0, "Max top curvature", 1)
    call assertf (maxval(airfoil%bot%curvature),110.2d0, "Max bot curvature", 1)

    ! write(*,"('top curv LE: ',200f7.1)") ( airfoil%top%curvature(i), i=1,10 )
    ! write(*,"('bot curv LE: ',200f7.1)") ( airfoil%bot%curvature(i), i=1,10 )

    ! write(*,"('top curv TE: ',200f6.2)") ( airfoil%top%curvature(i), i=92,101 )
    ! write(*,"('bot curv TE: ',200f6.2)") ( airfoil%bot%curvature(i), i=92,101)

    ! --- now rebuild airfoil again 

    new_airfoil = airfoil 

    call build_from_sides (new_airfoil)
    call assertf (sum(airfoil%x), sum(new_airfoil%x), "Rebuilded airfoil x coordinates ")
    call assertf (sum(airfoil%y), sum(new_airfoil%y), "Rebuilded airfoil z coordinates ")


  end subroutine


  
  subroutine test_airfoil_normalize  ()

    !! test of split airfoil into top and bot 

    use math_util,            only : point_type
    use airfoil_base,         only : le_of_spline, te_point, set_as_dat_based, repanel

    type(airfoil_type)              :: airfoil, new_airfoil 
    type(point_type)                :: le

    call test_header ("Airfoil normalize")

    airfoil = create_bezier_example_airfoil (201)
    call set_as_dat_based (airfoil)                     ! so spline will be used ...

    call split_foil_into_sides (airfoil) 

    call assertf (airfoil%top%curvature(1), 82.7d0, "le top curvature before ", 1)

    new_airfoil = repanel (airfoil)

    call asserti (size(new_airfoil%x), 161, "No of points for curvature results")

    call assertf (new_airfoil%top%curvature(1), 79.1d0, "le top curvature after  ", 1)

    le = le_of_spline (new_airfoil)
    call assertf (le%x, 0d0, "le x = 0.0 ", 7)
    call assertf (le%y, 0d0, "le y = 0.0 ", 7)

  end subroutine



  subroutine test_airfoil_geometry  ()

    !! test of geometry info like thickness 

    use airfoil_base,       only : te_gap
    use airfoil_geometry,   only : get_geometry, set_geometry
    use airfoil_geometry,   only : set_geometry_by_scale, set_te_gap, te_angle
    use airfoil_geometry,   only : eval_y_on_x
    use airfoil_base,       only : create_bezier_MH30, airfoil_write_with_shapes
    use airfoil_base,       only : set_as_dat_based, repanel

    double precision, allocatable   :: top_x(:), bot_x(:)
    type(airfoil_type)              :: airfoil, new_airfoil 
    double precision                :: t, xt, c, xc, a
    double precision                :: y_spline, y_bezier 
 
    call test_header ("Airfoil geometry")

    airfoil = create_bezier_MH30 (201)
    call airfoil_write_with_shapes (airfoil, "")

    ! clear bezier flag since we will normalize and modify geometry in this test
    call set_as_dat_based (airfoil)

    new_airfoil = repanel (airfoil)   ! just repanel to get normalized coordinates - geometry should be the same

    top_x = new_airfoil%top%x
    bot_x = new_airfoil%bot%x

    ! get geometry 

    call get_geometry (new_airfoil, t, xt, c, xc) 

    call asserti (size(new_airfoil%x), 161, "No of points for geometry results")
    call assertf (t,  0.078567d0, "Max thickness     "//strf('(F7.4)', t*1d2,.true.)//"%", 6)
    call assertf (xt, 0.292804d0, "Max thickness pos "//strf('(F7.4)', xt*1d2,.true.)//"%", 6)
    call assertf (c,  0.017040d0, "Max camber        "//strf('(F7.4)', c*1d2,.true.)//"%", 6)
    call assertf (xc, 0.454160d0, "Max camber pos    "//strf('(F7.4)', xc*1d2,.true.)//"%", 6)

    a = te_angle (new_airfoil)
    call assertf (a,  7.344822d0, "TE angle          "//strf('(F5.1)', a,.true.)//"deg", 6)

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
    call assertf (xc,   0.70000d0, "Scale camber pos    "//strf('(F7.4)',  xc*1d2)//"%", 5)

    ! set te gap 
    
    call set_te_gap (new_airfoil, 0.001d0)
    call assertf (te_gap (new_airfoil), 0.001d0, "Set te gap "//strf('(F5.2)', 0.001d0*1d2)//"%", 5)

    ! check x coordinates didn't change 

    call assertf (sum(new_airfoil%top%x),  sum(top_x), "Check top x didn't change", 6)
    call assertf (sum(new_airfoil%bot%x),  sum(bot_x), "Check bot x didn't change", 6)


    ! check eval y on x 
    
    call split_foil_into_sides (airfoil)          ! will create spline 

    y_spline = eval_y_on_x (airfoil%top, 0.4d0, airfoil%spl) 
    y_bezier = eval_y_on_x (airfoil%top, 0.4d0, airfoil%spl) 

    call assertf (y_spline,  0.054681d0, "Eval y on x spline", 6)
    call assertf (y_bezier,  0.054681d0, "Eval y on x bezier", 6)

  end subroutine



  subroutine test_airfoil_load_dat()

    use os_util, only : delete_file

    type(airfoil_type)            :: airfoil, loaded_airfoil
    character(:), allocatable     :: file_name, airfoil_name

    call test_header("Airfoil dat load")

    airfoil = create_bezier_example_airfoil(101)
    file_name = "test_airfoil_load"
    airfoil_name = "Roundtrip DAT Test"

    call airfoil_write_dat(file_name, airfoil_name, airfoil%x, airfoil%y)
    loaded_airfoil = airfoil_load_dat(file_name//'.dat')
    call delete_file(file_name//'.dat')

    call assert(allocated(loaded_airfoil%name), "Loaded dat airfoil name allocated")
    call assert(loaded_airfoil%name == airfoil_name, "Loaded dat airfoil name preserved")
    call asserti(size(loaded_airfoil%x), size(airfoil%x), "Loaded dat airfoil point count")
    call assertf(sum(loaded_airfoil%x), sum(airfoil%x), "Loaded dat x checksum", 6)
    call assertf(sum(loaded_airfoil%y), sum(airfoil%y), "Loaded dat y checksum", 6)

  end subroutine


end module 

