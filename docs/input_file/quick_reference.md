---
layout: home
title: Quick Reference
parent: Input File
nav_order: 2
---

# Quick Reference
{: .no_toc }



## All parameters 

```f90
!
! Xoptfoil2 - Reference of input parameters 
!
! Values shown are the default values of the parameters
!

&optimization_options
  airfoil_file     = 'mySeedAirfoil.dat'              ! either .dat or .bez file 
  shape_functions  = 'hicks-henne'                    ! either 'hicks-henne', 'bezier' or 'camb-thick' 
  cpu_threads      = -1                               ! no of cpu threads to be used, negative means n less than available 
  show_details     = .true.                           ! show details of actions and results           
/  


&hicks_henne_options
  nfunctions_top   = 4                                ! hicks-henne functions on top side              
  nfunctions_bot   = 4                                ! hicks-henne functions on bot side
  initial_perturb  = 0.1                              ! max. perturb of when creating initial designs 
  smooth_seed      = .false.                          ! smooth (match bezier) of seed airfoil prior to optimization
/


&bezier_options
  initial_perturb  = 0.1                              ! equals approx. 10% of solution space - better less
  ncp_top          = 6                                ! bezier control points on top side              
  ncp_bot          = 6                                ! bezier control points on bot side
/

&camb_thick_options
  thickness        = .true.                           ! optimize thickness 
  thickness_pos    = .true.                           ! optimize max. thickness position
  camber           = .true.                           ! optimize camber
  camber_pos       = .true.                           ! optimize max. camber position
  le_radius        = .true.                           ! optimize leading edge radius
  le_radius_blend  = .true.                           ! blending distance for le radius change 
  initial_perturb  = 0.1d0                            ! max. perturb of when creating initial designs 
/


&panelling_options
  npan             = 160                              ! no of panels of airfoil
  npoint           = 161                              ! alternative: number of coordinate points
  le_bunch         = 0.86                             ! panel bunch at LE - 0..1 (max) 
  te_bunch         = 0.6                              ! panel bunch at TE - 0..1 (max) 
/


&particle_swarm_options
  pop              = 30                               ! swarm population - no of particles 
  min_radius       = 0.001                            ! design radius when optimization shall be finished
  max_iterations   = 500                              ! max no of iterations 
  max_retries      = 3                                ! no of retries of a particle if it fails because of geometry
  max_speed        = 0.1                              ! max speed of a perticle - equals approx 10% of solution space 
  init_attempts    = 1000                             ! no of tries to get initial, valid design 
  convergence_profile = 'exhaustive'                  ! either 'exhaustive' or 'quick' or 'quick_camb_thick'
/


&operating_conditions
  dynamic_weighting      = .true.                     ! activate dynamic weighting during optimization
  allow_improved_target  = .true.                     ! result allowed to be better than target 
  
  re_default             = 400000                     ! use this Reynolds number for operating points
  re_default_as_resqrtcl = .false.                    ! take the re number as type 2 
  mach_default          = 0.0                         ! use this mach number for operating points 
  
  use_flap               = .false.                    ! activate flap setting or optimization
  x_flap                 = 0.75                       ! chord position of flap 
  y_flap                 = 0.0                        ! vertical hinge position 
  y_flap_spec            = 'y/c'                      ! ... in chord unit or 'y/t' relative to height at hinge
  flap_angle_default     = 0.0                        ! default flap angle for all op points

  noppoint         = 0                                ! no of operating points

! --- repeat this per operating point ----------------
  
  op_mode(1)       = 'spec_cl'                        ! op either 'spec_cl' or 'spec_al' based                              
  op_point(1)      = 0.0                              ! value of either cl or alpha         
  optimization_type(1) = 'target-drag'                ! 'min-drag', 'max-glide', 'min-sink', 'max-lift', 'max-xtr', 
                                                      ! 'target-moment', 'target-drag', 'target-glide'
  target_value(1)  = 0.0                              ! target value if type = 'target-...'              
  weighting(1)     = 1.0                              ! weighting  
  reynolds(1)      = ...                              ! individual re number of op (default: re_default) 
  mach(1)          = ...                              ! individual mach number of op (default: mach_default) 
  ncrit_pt(1)      = ...                              ! individual ncrit of op (default: ncrit of &xfoil_run_options) 

  flap_angle(1)    = ...                              ! individual flap angle (default: flap_angle_default)
  flap_optimize(1) = .false.                          ! optimize flap angle of this op point
/


&geometry_targets
  ngeo_targets     = 0                                ! no of geometry targets 
  target_type(1)   = ''                               ! either 'camber', 'thickness' 
  target_geo(1)    = 0.0                              ! target value to achieve 
  weighting_geo(1) = 1.0                              ! weighting of this target
  preset_to_target(1) = .false.                       ! preset seed airfoil to this target before optimization  
/  


&curvature
  check_curvature  = .true.                           ! check curvature during optimization
  auto_curvature   = .true.                           ! auto determine thresholds for curvature and bumps
  max_curv_reverse_top = 0                            ! max no of curvature reversals - top 
  max_curv_reverse_bot = 0                            ! max no of curvature reversals - bot 
  max_te_curvature = 10.0                             ! max curvature at trailing edge
  max_le_curvature_diff = 4.0                         ! Bezier: max curvature difference at leading edge
  curv_threshold   = 0.1                              ! threshold to detect reversals 
  spike_threshold  = 0.4                              ! threshold to detect spikes aga bumps 
  max_spikes_top   = 0                                ! max no of curvature bumbs - top 
  max_spikes_bot   = 0                                ! max no of curvature bumbs - bot 
/


&constraints
  check_geometry   = .true.                           ! check geometry againt basic constraints 
  min_te_angle     = 2.d0                             ! min trailing edge angle in degrees
  symmetrical      = .false.                          ! force to be symmetrical 
  min_thickness    = NOT_DEF_D                        ! min thickness (better use targets) 
  max_thickness    = NOT_DEF_D                        ! max thcikness (better use targets) 
  min_camber       = NOT_DEF_D                        ! min camber (better use targets) 
  max_camber       = NOT_DEF_D                        ! max camver (better use targets) 


&polar_generation                                     ! only for 'Worker'   
  type_of_polar   = 1                                 ! either Type 1 or Type 2 polar 
  op_mode         = 'spec-al'                         ! range based on alpha or cl 
  op_point_range  = -2, 10, 0.25                      ! range start, end, delta 
  polar_reynolds  = 0                                 ! list of reynolds like 100000, 200000, 600000
/

&xfoil_run_options
  ncrit            = 7                                ! ncrit default value for op points 
  xtript           = 1.0                              ! forced transition point - top  
  xtripb           = 1.0                              ! forced transition point - bot  
  bl_maxit         = 50                               ! max no of xfoil iterations to converge
  vaccel           = 0.005                            ! xfoil vaccel
  fix_unconverged  = .true.                           ! auto retry when op point doesn't converge
  reinitialize     = .false.                          ! reinit xfoil bl after each op point 
/
```
