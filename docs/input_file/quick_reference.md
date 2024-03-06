---
layout: home
title: Quick Reference
parent: Input File
nav_order: 2
---

# Quick Reference
{: .no_toc }

All namelists with their parameters currently available. Only a short explanation is given for each paramter. 
See [Input Reference](input_file.md) for more details. 

{: .note }
Copy & Paste single paramters or complete namelists from here into ypur input file 


```fortran
!
! Xoptfoil2 - Reference of input parameters 
!
! Values shown are the default values of the parameters
!

&optimization_options                            ! main control of optimization
  airfoil_file     = '<mySeedAirfoil>'           ! either '.dat', '.bez' or '.hicks' file 
  shape_functions  = 'hicks-henne'               ! either 'hicks-henne', 'bezier' or 'camb-thick' 
  cpu_threads      = -1                          ! no of cpu threads or -n less than available 
  show_details     = .true.                      ! show details of actions and results           
/  

&hicks_henne_options                             ! options for shape_function 'hicks-henne'
  nfunctions_top   = 3                           ! hicks-henne functions on top side              
  nfunctions_bot   = 3                           ! hicks-henne functions on bot side
  initial_perturb  = 0.1                         ! max. perturb when creating initial designs 
  smooth_seed      = .false.                     ! smooth (match bezier) of seed airfoil prior to optimization
/

&bezier_options                                  ! options for shape_function 'bezier'
  ncp_top          = 6                           ! no of bezier control points on top side              
  ncp_bot          = 6                           ! no of bezier control points on bot side
  initial_perturb  = 0.1                         ! max. perturb when creating initial designs
/

&camb_thick_options                              ! options for shape_function 'camb_thick'
  thickness        = .true.                      ! optimize thickness 
  thickness_pos    = .true.                      ! optimize max. thickness position
  camber           = .true.                      ! optimize camber
  camber_pos       = .true.                      ! optimize max. camber position
  le_radius        = .true.                      ! optimize leading edge radius
  le_radius_blend  = .true.                      ! optimize blending distance for le radius change 
  initial_perturb  = 0.1d0                       ! max. perturb when creating initial designs 
/

&operating_conditions                            ! options to describe the optimization task
  dynamic_weighting      = .true.                ! activate dynamic weighting during optimization
  allow_improved_target  = .true.                ! allow result to be better than target value
  
  re_default             = 400000                ! use this Reynolds number for operating points
  re_default_as_resqrtcl = .false.               ! interpret re number as type 2 (Re*sqrt(cl)) 
  mach_default           = 0.0                   ! use this Mach number for operating points 
  
  use_flap               = .false.               ! activate flap setting or optimization
  x_flap                 = 0.75                  ! chord position of flap 
  y_flap                 = 0.0                   ! vertical hinge position 
  y_flap_spec            = 'y/c'                 ! ... in chord unit or 'y/t' relative to height
  flap_angle_default     = 0.0                   ! default flap angle for all op points

  noppoint         = 0                           ! no of operating points

  ! --- repeat this per operating point ---------
  
  op_mode(1)       = 'spec_cl'                   ! op either 'spec_cl' or 'spec_al' based             
  op_point(1)      = 0.0                         ! value of either cl or alpha         
  optimization_type(1) = 'target-drag'           ! 'min-drag', 'max-glide', 'min-sink', 
                                                 ! 'max-lift', 'max-xtr', 
                                                 ! 'target-drag', 'target-glide', 'target-moment', 
  target_value(1)  = 0.0                         ! target value if type = 'target-...'              
  weighting(1)     = 1.0                         ! weighting during optimization 
  reynolds(1)      = ...                         ! individual re number of op (default: re_default) 
  mach(1)          = ...                         ! individual mach number of op (default: mach_default) 
  ncrit_pt(1)      = ...                         ! individual ncrit of op  

  flap_angle(1)    = ...                         ! individual flap angle (default: flap_angle_default)
  flap_optimize(1) = .false.                     ! optimize this flap angle 
/

&geometry_targets                                ! geometry targets which should be achieved
  ngeo_targets     = 0                           ! no of geometry targets 
  target_type(1)   = ''                          ! either 'camber', 'thickness' 
  target_geo(1)    = 0.0                         ! target value to achieve 
  weighting_geo(1) = 1.0                         ! weighting of this target
  preset_to_target(1) = .false.                  ! preset seed airfoil to this target 
/  

&curvature                                       ! geometry curvature constraints for optimization 
  check_curvature  = .true.                      ! check curvature during optimization
  auto_curvature   = .true.                      ! auto determine thresholds for curvature and bumps
  max_curv_reverse_top = 0                       ! max no of curvature reversals - top ("reflexed"?)
  max_curv_reverse_bot = 0                       ! max no of curvature reversals - bot ("rearloading"?)
  curv_threshold   = 0.1                         ! threshold to detect reversals 
  max_te_curvature = 10.0                        ! max curvature at trailing edge
  max_le_curvature_diff = 5.0                    ! Bezier: max curvature difference at leading edge
  spike_threshold  = 0.5                         ! threshold to detect spikes aga bumps 
  max_spikes_top   = 0                           ! max no of curvature bumbs - top 
  max_spikes_bot   = 0                           ! max no of curvature bumbs - bot 
/

&constraints                                     ! geometry constraints for optimization
  check_geometry   = .true.                      ! check geometry against geometry constraints 
  min_te_angle     = 2.d0                        ! min trailing edge angle in degrees
  symmetrical      = .false.                     ! force airfoil to be symmetrical 
  min_thickness    = ...                         ! min thickness        (better use geometry targets) 
  max_thickness    = ...                         ! max thickness        (better use geometry targets) 
  min_camber       = ...                         ! min camber           (better use geometry targets) 
  max_camber       = ...                         ! max camver           (better use geometry targets) 

&panelling_options                               ! options for re-panelling before optimization 
  npan             = 160                         ! no of panels of airfoil
  npoint           = 161                         ! alternative: number of coordinate points
  le_bunch         = 0.86                        ! panel bunch at leading edge  - 0..1 (max) 
  te_bunch         = 0.6                         ! panel bunch at trailing edge - 0..1 (max) 
/

&particle_swarm_options                          ! options for particle swarm optimization - PSO
  pop              = 30                          ! swarm population - no of particles 
  min_radius       = 0.001                       ! design radius when optimization shall be finished
  max_iterations   = 500                         ! max no of iterations 
  max_retries      = 3                           ! no of particle retries for geometry violations
  max_speed        = 0.1                         ! max speed of a particle in solution space 0..1 
  init_attempts    = 1000                        ! no of tries to get initial, valid design 
  convergence_profile = 'exhaustive'             ! either 'exhaustive' or 'quick' or 'quick_camb_thick'
/

&polar_generation                                ! options only for 'Worker'   
  type_of_polar   = 1                            ! either Type 1 or Type 2 polar 
  op_mode         = 'spec-al'                    ! range based on alpha or cl 
  op_point_range  = -2, 10, 0.25                 ! range start, end, delta 
  polar_reynolds  = 0                            ! list of reynolds like 100000, 200000, 600000
/

&xfoil_run_options
  ncrit            = 7                           ! ncrit default value for op points 
  xtript           = 1.0                         ! forced transition point 0..1 - top  
  xtripb           = 1.0                         ! forced transition point 0..1 - bot  
  bl_maxit         = 50                          ! max no of xfoil iterations to converge
  vaccel           = 0.005                       ! xfoil vaccel paramter
  fix_unconverged  = .true.                      ! auto retry when op point doesn't converge
  reinitialize     = .false.                     ! reinit xfoil boundary layer after each op point 
/
```
