! MIT License
! Copyright (c) 2024 Jochen Guenzel

module test_airfoil_evals
  
  !-------------------------------------------------------------------------
  ! Airfoil evaluations with geometry, curvature constraints 
  !-------------------------------------------------------------------------

  use os_util
  use test_util 
  use airfoil_base,       only : airfoil_type
  use airfoil_base,       only : split_foil_into_sides, build_from_sides
  use shape_bezier,       only : bezier_spec_type, create_bezier_MH30
  use eval_commons,       only : geo_constraints_type
  use eval_constraints,   only : eval_geometry_violations, max_panels_angle

  implicit none

  contains

  subroutine test_eval_constraints  ()

    !! test of geometry constraints 

    character (:), allocatable      :: name 
    double precision, allocatable   :: x(:), y(:)
    type(airfoil_type)              :: airfoil
    type(bezier_spec_type)          :: top_bezier, bot_bezier 
    type (geo_constraints_type)     :: geo 
    logical                         :: has_violation 
    character (100)                 :: info

    call test_header ("Airfoil constraints")

    call create_bezier_MH30 (201, name, x, y, top_bezier, bot_bezier)

    airfoil%x = x
    airfoil%y = y
    airfoil%name = name
    airfoil%symmetrical = .false. 

    call split_foil_into_sides (airfoil) 

    ! airfoil data 

    call assertf (max_panels_angle(airfoil), 12.2d0, "Max panels angle", 1)
    ! constraints 

    geo%check_geometry = .true.
    geo%min_thickness = NOT_DEF_D
    geo%max_thickness = NOT_DEF_D
    geo%min_te_angle = 2d0
    geo%min_camber = NOT_DEF_D
    geo%max_camber = NOT_DEF_D   

    call eval_geometry_violations (airfoil, geo, has_violation, info)

    call asserti (len(trim(info)), 0, "No violations - no text: "//trim(info))

    geo%max_thickness = 0.07d0
    call eval_geometry_violations (airfoil, geo, has_violation, info)
    call asserti (len(trim(info)), 40, "Violated max thickness - text: "//trim(info))

    geo%max_thickness = NOT_DEF_D
    geo%min_camber = 0.02d0
    call eval_geometry_violations (airfoil, geo, has_violation, info)
    call asserti (len(trim(info)), 37, "Violated min camber - text: "//trim(info))

  end subroutine

end module 

