! MIT License
! Copyright (c) 2024 Jochen Guenzel

!
! common types of airfoil evaluation modules 
!

module eval_commons

  use airfoil_operations,   only : airfoil_type, panel_options_type
  use xfoil_driver,         only : xfoil_options_type
  use xfoil_driver,         only : op_point_spec_type, re_type
  use xfoil_driver,         only : op_point_result_type
  use xfoil_driver,         only : flap_spec_type

  implicit none
  private

  public :: eval_spec_type

  public :: geo_target_type
  public :: geo_result_type

  public :: geo_constraints_type
  public :: curv_side_constraints_type
  public :: curv_constraints_type

  public :: dynamic_weighting_spec_type


  ! Defines a geometric target eg thickness of the optimization 

  type geo_target_type  
    character(:), allocatable :: type           ! eg 'Thickness'
    logical          :: preset_to_target          ! preset seed airfoil to this target before optimization
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

  type dynamic_weighting_spec_type                              
    logical          :: active                  ! do dynamic weighting
    double precision :: min_weighting           ! min. value of weighting e.g. 0.5
    double precision :: max_weighting           ! max. value of weighting e.g. 4
    double precision :: extra_punch             ! extra weighting punch if deviation is too high
    integer          :: frequency               ! recalc weighting every n designs
    integer          :: start_with_design       ! dynamic weighting will start with design #...
  end type  

  ! geometry constraints

  type geo_constraints_type
    logical             :: check_geometry
    logical             :: symmetrical
    double precision    :: min_thickness
    double precision    :: max_thickness 
    double precision    :: min_te_angle
    double precision    :: min_camber
    double precision    :: max_camber
    ! double precision, allocatable :: addthick_x(:), addthick_min(:), addthick_max(:) 
  end type

  ! curvature constraints - common and per side 

  type curv_side_constraints_type
    logical          :: check_curvature_bumps   ! check for bumps (curvature derivative reversals)
    double precision :: curv_threshold          ! threshold to detetc reversals of curvature
    double precision :: spike_threshold         ! threshold to detetc reversals of curv derivative
    integer          :: max_curv_reverse        ! max. number of reversals 
    integer          :: max_spikes              ! max. number of spikes 
    double precision :: max_te_curvature        ! max. curvature at trailing edge
    integer          :: nskip_LE = 1            ! no of ponts to skip when scanning
  end type curv_side_constraints_type                           

  type curv_constraints_type              
    logical          :: check_curvature         ! check curvature during optimization
    logical          :: auto_curvature          ! best thresholds will be determined
    double precision :: max_le_curvature_diff   ! Bezier: allowed diff of le curvature on top and bot 
    type (curv_side_constraints_type)  :: top   ! top side curvature 
    type (curv_side_constraints_type)  :: bot   ! bottom side curvature 
  end type curv_constraints_type                             


  ! super type for all evaluation specificationn sub-types (used for easy handling)  

  type eval_spec_type 

    ! operating points 
    integer             :: noppoint              
    type (op_point_spec_type), allocatable  :: op_points_spec (:)

    ! geometry targets
    type(geo_target_type), allocatable      :: geo_targets (:)

    ! Geometry and curvature constraints
    type (geo_constraints_type)             :: geo_constraints 
    type (curv_constraints_type)            :: curv_constraints
  
    ! flap usage 
    type (flap_spec_type)                   :: flap_spec

    ! dynamic weighting 
    type (dynamic_weighting_spec_type)      :: dynamic_weighting_spec 

    ! Match foil mode
    character (:), allocatable              :: foil_to_match_name
    type(airfoil_type)                      :: foil_to_match 
    logical                                 :: match_foils
    double precision                        :: match_foils_scale_factor 
  
    ! Geometry options for repanelling
    type (panel_options_type)               :: panel_options 

    ! Xfoil options
    type(xfoil_options_type)                :: xfoil_options
  
  end type 

contains

end module 