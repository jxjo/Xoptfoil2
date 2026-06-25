! MIT License
! Copyright (c) 2025 Jochen Guenzel

!
! common types of airfoil evaluation modules 
!

module eval_commons

  use os_util,              only : NOT_DEF_D
  use math_util,            only : point_type
  use airfoil_base,         only : airfoil_type, panel_options_type
  use geo_target,           only : geo_target_type
  use xfoil_driver,         only : xfoil_options_type
  use xfoil_driver,         only : flap_spec_type
  use op_point,             only : op_point_spec_type, re_type, op_point_result_type

  implicit none
  private

  public :: eval_spec_type
  public :: geo_constraints_type
  public :: curv_side_constraints_type
  public :: curv_constraints_type
  public :: goal_attainment_type
  public :: MAX_PENALTY_DESIGN_VALID, MAX_PENALTY_DESIGN_FAIL

  double precision, parameter :: MAX_PENALTY_DESIGN_VALID = 0.0001d0 
  double precision, parameter :: MAX_PENALTY_DESIGN_FAIL  = 0.02d0 

  ! geometry constraints

  type geo_constraints_type
    logical             :: check_geometry
    logical             :: symmetrical
    double precision    :: min_thickness
    double precision    :: max_thickness 
    double precision    :: min_te_angle
    double precision    :: min_te_top_angle
    double precision    :: max_te_bot_angle
    double precision    :: min_camber
    double precision    :: max_camber
    type (point_type)   :: min_thickness_at_x = point_type(NOT_DEF_D, NOT_DEF_D)
    double precision    :: seed_penalty = 0d0
  end type

  ! curvature constraints - common and per side 

  type curv_side_constraints_type
    logical          :: check_curvature_bumps = .true.  ! check for bumps (curvature derivative reversals)
    logical          :: check_le_curvature = .true.     ! check curvature at LE is monotonous
    double precision :: curv_threshold = 0.01d0         ! threshold to detect reversals of curvature
    integer          :: max_curv_reverse = 0            ! max. number of reversals 
    double precision :: max_te_curvature = 0.1d0        ! max. curvature at trailing edge
  end type curv_side_constraints_type                           

  type curv_constraints_type             
    logical          :: check_curvature = .true.        ! check curvature during optimization
    logical          :: auto_curvature = .true.         ! best thresholds will be determined
    type (curv_side_constraints_type)  :: top           ! top side curvature 
    type (curv_side_constraints_type)  :: bot           ! bottom side curvature 
    double precision :: seed_penalty = 0d0
  end type curv_constraints_type                             


  type goal_attainment_type
    double precision :: weighting_user = 1d0          ! user weighting before adjustment 
    double precision :: weighting_user_eff = 0d0      ! effective user weighting after auto adjustment
    double precision :: weighting      = 0d0          ! normalized weighting in objective
    double precision :: seed_value     = 0d0          ! raw seed reference for normalization
  end type goal_attainment_type
 

  ! -------------------------------------------------------------------------------
  ! super type for all evaluation specificationn sub-types (used for easy handling)  
  ! -------------------------------------------------------------------------------

  type eval_spec_type  

    type (op_point_spec_type), allocatable  :: op_point_specs (:)
    type (geo_target_type), allocatable     :: geo_targets (:)
    type (goal_attainment_type)             :: goal_attainment
    type (geo_constraints_type)             :: geo_constraints 
    type (curv_constraints_type)            :: curv_constraints
    type (panel_options_type)               :: panel_options 
    type (xfoil_options_type)               :: xfoil_options
  
  end type 

contains

end module 