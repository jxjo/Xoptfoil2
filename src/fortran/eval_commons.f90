! MIT License
! Copyright (c) 2024 Jochen Guenzel

!
! common types of eairfoil evaluation modules 
!

module eval_commons

  use commons,      only : airfoil_type
  use xfoil_driver, only : xfoil_options_type, xfoil_geom_options_type
  use xfoil_driver, only : op_point_specification_type, re_type
  use xfoil_driver, only : op_point_result_type
  use airfoil_constraints

  implicit none
  public
  ! Defines a geometric target eg thickness of the optimization 

  type geo_target_type  
    character(:), allocatable :: type           ! eg 'Thickness'
    double precision :: target_value            ! target value to achieve
    double precision :: seed_value              ! the value of the seed airfoil
    double precision :: reference_value         ! to scale improvement (depends on type)
    double precision :: scale_factor            ! scale for objective function

    double precision :: weighting               ! weighting within objective function
    double precision :: weighting_user          ! original weighting entered by user
    logical          :: dynamic_weighting       ! dynamic weighting for this point 
    logical          :: extra_punch             !  - this op got an extra weighting punch
    double precision :: weighting_user_cur      !  - info: current scaled user weighting
    double precision :: weighting_user_prv      !  - info: previous scaled user weighting
  end type geo_target_type

  type geo_result_type                           ! results of geometry evaluation 
    double precision   :: maxt, xmaxt            ! thickness of airfoil
    double precision   :: maxc, xmaxc            ! camber of airfoil
    double precision   :: top_curv_le, top_curv_te   ! curvature at le and te 
    double precision   :: bot_curv_le, bot_curv_te  
  end type geo_result_type         

  ! Dynamic weighting of operating points and geo targets during optimization 

  type dynamic_weighting_specification_type                              
    logical          :: active                  ! do dynamic weighting
    double precision :: min_weighting           ! min. value of weighting e.g. 0.5
    double precision :: max_weighting           ! max. value of weighting e.g. 4
    double precision :: extra_punch             ! extra weighting punch if deviation is too high
    integer          :: frequency               ! recalc weighting every n designs
    integer          :: start_with_design       ! dynamic weighting will start with design #...
  end type dynamic_weighting_specification_type

  type dynamic_variable_type
    double precision  :: dev  
    double precision  :: weighting, new_weighting 
  end type 


! Geometry and curvature constraints

  type (geo_constraints_type)     :: geo_constraints 
  type (curv_constraints_type)    :: curv_constraints


! Geo targets 

  integer, parameter :: max_geo_targets = 10
  type(geo_target_type), dimension (:), allocatable  :: geo_targets

  
! Parms for operating point specification

  integer :: noppoint
  type (op_point_specification_type), dimension (:), allocatable :: op_points_spec 
  type (dynamic_weighting_specification_type)  :: dynamic_weighting_spec 

! Match foil mode
  type(airfoil_type) :: foil_to_match 
  logical :: match_foils
  double precision :: match_foils_scale_factor 

! Xfoil options
  type(xfoil_options_type)       :: xfoil_options
  type(xfoil_geom_options_type)  :: xfoil_geom_options


contains

end module 