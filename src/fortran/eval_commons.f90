! MIT License
! Copyright (c) 2025 Jochen Guenzel

!
! common types of airfoil evaluation modules 
!

module eval_commons

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
  public :: match_foil_spec_type
  public :: goal_attainment_type

  ! geometry constraints

  type geo_constraints_type
    logical             :: check_geometry
    logical             :: symmetrical
    double precision    :: min_thickness
    double precision    :: max_thickness 
    double precision    :: min_te_angle
    double precision    :: min_camber
    double precision    :: max_camber
  end type

  ! curvature constraints - common and per side 

  type curv_side_constraints_type
    logical          :: check_curvature_bumps = .true.  ! check for bumps (curvature derivative reversals)
    logical          :: check_le_curvature = .true.     ! check curvature at LE is monotonous
    double precision :: curv_threshold = 0.01d0         ! threshold to detetc reversals of curvature
    double precision :: initial_penalty = 0.0d0         ! initial penalty of seed foil
    integer          :: max_curv_reverse = 0            ! max. number of reversals 
    double precision :: max_te_curvature = 0.1d0        ! max. curvature at trailing edge
  end type curv_side_constraints_type                           

  type curv_constraints_type             
    logical          :: check_curvature = .true.        ! check curvature during optimization
    logical          :: auto_curvature = .true.         ! best thresholds will be determined
    type (curv_side_constraints_type)  :: top           ! top side curvature 
    type (curv_side_constraints_type)  :: bot           ! bottom side curvature 
  end type curv_constraints_type                             


  ! match foil specs 

  type match_foil_spec_type 
    logical                            :: active = .false.  ! match-foil mode active 
    character(:), allocatable          :: filename          ! filename of foil to match 
    type (airfoil_type)                :: foil              ! original airfoil to match 
  end type match_foil_spec_type 


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
    type (match_foil_spec_type)             :: match_foil_spec
  
  end type 

contains

end module 