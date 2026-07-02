---
layout: home
title: Input File
nav_order: 3
has_children: false
parent: Run Xoptfoil2
permalink: docs/input_file
---

# Input File 
{: .no_toc}

As Xoptfoil2 does not have a user interface, all settings and parameters for an optimization task are made via an input file.
{: .fs-6 .fw-300 }

When starting Xoptfoil2 the input file is read and checked for consistency. If an error is detected program execution will be stopped at this early stage. 

If all parameters are consistent, the seed airfoil is loaded and pre-processed. The optimizer then starts the iteration loop.


### Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

---

## Format of the Input File 

The parameters of the input file are grouped in sections called ‘namelists’ which start with `&namelist_name` and end with an `/` character. The format of the input file is also an indication of the language in which the program is written: Fortran.   

```fortran
&option_list
  logical_option  = .true.                      ! this is a remark 
  number_option   = 1.0 
  string_option   = 'bezier' 
/
```
{: .lh-tight }

An option can either be  
- logical with the values `.true.` or `.false.`
- numerical with a point as decimal separator  
- a string enclosed in apostrophes like `'myString'`

Remarks or comment are introduced by an exclamation mark. It is a good advice to use 'remarks' extensively in your input file to remember modifications made between several optimizations runs.



### Minimum example

Only a few parameters are mandatory and most of the numerous Xoptfoil2 parameters are optional having a default value. Here is an example of a minimum input file for a valid optimization task 

```fortran
! Optimize cd while trying to preserve glide ratio of SD7003 at Reynolds 400,000 

&optimization_options
  airfoil_file     = 'SD7003.dat'                ! the seed airfoil 
  shape_functions  = 'bezier'                    ! shape functions: 'hicks-henne', 'bezier' or 'bspline'
/  

&operating_conditions
  re_default       = 400000                      ! Reynolds number for all operating points
  noppoint         = 2                           ! we define only 2 operating points

  op_mode(1) = 'spec-cl'                         ! operating point based on cl-value
  op_point(1) = 0.2                              ! cl=0.2
  optimization_type(1) = 'min-drag'              ! minimize cd (drag) at this point 

  op_mode(2) = 'spec-cl'                         ! operating point based on cl-value
  op_point(2) = 0.7                              ! cl = 0.7
  optimization_type(2) = 'target-glide'          ! target is the glide ratio cl/cd  
  target_value(2) = 74.0                         ! example target glide value
/
```
{: .lh-tight }

### Parameters as arguments 

There are a few optimization parameters which can be provided as command line arguments. 
In this case the command line argument will overwrite the value in the input file. 

- `-a <filename>`  overwrites `<airfoil_file>` of `&optimization_options`
- `-r <number>`    overwrites `<re_default>` of `&operating_conditions`

Using command line arguments can be useful when a row of optimization tasks should be automized for example to build a series of airfoils for a single wing. 


## Quick Reference


Overview of all namelists currently supported by the code. Defaults below are practical starting values; use the chapter links at the end for deeper explanations and usage guidance.

{: .tip }
Copy & Paste single parameters or complete namelists from here into your input file 


```fortran
!
! Xoptfoil2 - Reference of input parameters 
!
! Values shown are the default values of the parameters
! Some options are marked deprecated but still accepted for compatibility.
!

&optimization_options                            ! main control of optimization
  airfoil_file     = '<mySeedAirfoil>'           ! either '.dat', '.bez', '.bsp' or '.hicks' file
  shape_functions  = 'bezier'                    ! either 'hicks-henne', 'bezier' or 'bspline'
  cpu_threads      = -1                          ! no of cpu threads or -n less than available 
  show_details     = .true.                      ! show details of actions and results           
  wait_at_end      = .false.                     ! wait for user input at end of run
/  

&hicks_henne_options                             ! options for shape_function 'hicks-henne'
  nfunctions_top   = 3                           ! hicks-henne functions on top side              
  nfunctions_bot   = 3                           ! hicks-henne functions on bot side
  initial_perturb  = 0.1                         ! max. perturb when creating initial designs 
/

&bezier_options                                  ! options for shape_function 'bezier'
  ncp_top          = 6                           ! no of bezier control points on top side
  ncp_bot          = 6                           ! no of bezier control points on bot side
  initial_perturb  = 0.1                         ! max. perturb when creating initial designs
/

&bspline_options                                 ! options for shape_function 'bspline'
  ncp_top          = 7                           ! no of bspline control points on top side
  ncp_bot          = 7                           ! no of bspline control points on bot side
  initial_perturb  = 0.1                         ! max. perturb when creating initial designs
/

&operating_conditions                            ! options to describe the optimization task
  allow_improved_target = .true.                ! if .true., values better than target are accepted

  re_default             = 400000                ! use this Reynolds number for operating points
  re_default_as_resqrtcl = .false.               ! interpret re number as type 2 (Re*sqrt(cl)) 
  mach_default           = 0.0                   ! use this Mach number for operating points 
  
  use_flap               = .false.               ! activate flap setting or optimization
  x_flap                 = 0.75                  ! chord position of flap 
  y_flap                 = 0.0                   ! vertical hinge position 
  y_flap_spec            = 'y/t'                 ! ... in chord unit or 'y/t' relative to height
  flap_angle_default     = 0.0                   ! default flap angle for all op points

  noppoint         = 0                           ! no of operating points

  ! --- repeat this per operating point ---------
  
  op_mode(1)       = 'spec-cl'                   ! op either 'spec-cl' or 'spec-al' based             
  op_point(1)      = 0.0                         ! value of either cl or alpha         
  optimization_type(1) = 'target-drag'           ! 'min-drag', 'max-glide', 'min-sink', 
                                                 ! 'max-lift', 'max-xtr', 
                                                 ! 'target-drag', 'target-glide', 'target-moment',
                                                 ! 'target-cp-min'
  target_value(1)  = 0.0                         ! target value if type = 'target-...'              
  weighting(1)     = 1.0                         ! weighting during optimization 
  reynolds(1)      =                             ! individual re number of op (default: re_default) 
  mach(1)          =                             ! individual mach number of op (default: mach_default) 
  ncrit_pt(1)      =                             ! individual ncrit of op  

  flap_angle(1)    =                             ! individual flap angle (default: flap_angle_default)
  flap_optimize(1) = .false.                     ! optimize this flap angle 
/

&geometry_targets                                ! geometry targets which should be achieved
  ngeo_targets     = 0                           ! no of geometry targets 
  target_type(1)   = ''                          ! either 'camber', 'thickness' 
  target_value(1)  = 0.0                         ! target value to achieve 
  weighting(1)     = 1.0                         ! weighting of this target
/  

&curvature                                       ! geometry curvature constraints for optimization 
  check_curvature  = .true.                      ! check curvature during optimization
  auto_curvature   = .true.                      ! auto determine thresholds for curvature and bumps
  check_le_curvature = .true.                    ! ensure curvature decreases monotonically at the leading edge
  check_curvature_bumps = .true.                 ! suppress curvature bumps
  max_curv_reverse_top = 0                       ! max no of curvature reversals - top ("reflexed"?)
  max_curv_reverse_bot = 0                       ! max no of curvature reversals - bot ("rearloading"?)
  curv_threshold   = 0.01                        ! threshold to detect reversals
  max_te_curvature = 0.1                         ! max curvature at trailing edge
  bump_threshold  = 0.01                         ! threshold of curvature derivative to detect bumps
/

&constraints                                     ! geometry constraints for optimization
  check_geometry   = .true.                      ! check geometry against geometry constraints 
  min_te_angle     =                             ! min trailing edge angle in degrees
  symmetrical      = .false.                     ! force airfoil to be symmetrical 
  min_thickness    =                             ! min thickness        (better use geometry targets) 
  max_thickness    =                             ! max thickness        (better use geometry targets) 
  min_camber       =                             ! min camber           (better use geometry targets) 
  max_camber       =                             ! max camber           (better use geometry targets)
  min_thickness_at_x =                           ! min thickness at a given x position
  min_te_top_angle  =                            ! min trailing edge top angle
  max_te_bot_angle  =                            ! max trailing edge bottom angle
  min_flap_angle   = -5.d0                       ! minimum flap angle for flap optimization
  max_flap_angle   = 15.d0                       ! maximum flap angle for flap optimization
/

&paneling_options                                ! options for re-paneling before optimization 
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
  convergence_profile = 'exhaustive'             ! either 'exhaustive' or 'quick' 
/

&polar_generation                                ! options only for 'worker'   
  generate_polar   = .true.                      ! enable polar generation
  polar_reynolds   =                             ! list of reynolds like 100000, 200000, 600000
  polar_mach       = 0                           ! list of mach like 0.1, 0.2, 0.5
  type_of_polar    = 1                           ! if omitted: derived from re_default type
  auto_range       = .false.                     ! best values for mode and range automatically set
  op_mode          = 'spec-al'                   ! range based on alpha or cl 
  op_point_range   =                             ! range start, end, delta (auto/derived when omitted)
/

&xfoil_run_options
  ncrit            = 9                           ! ncrit default value for op points 
  xtript           = 1.0                         ! forced transition point 0..1 - top  
  xtripb           = 1.0                         ! forced transition point 0..1 - bot  
  viscous_mode     = .true.                      ! run in viscous mode
  silent_mode      = .true.                      ! suppress xfoil console chatter
  bl_maxit         = 40                          ! max no of xfoil iterations to converge
  vaccel           = 0.005                       ! xfoil vaccel parameter
  fix_unconverged  = .true.                      ! auto retry when op point doesn't converge
/
```
{: .lh-tight }


## Input Reference 

The quick reference above lists all input options with defaults. For deeper explanation and practical guidance, use the dedicated chapters:

- [Optimization Task]({% link airfoil_optimization/optimization_task.md %})
  - detailed explanation of operating points, optimization types, targets, weighting, and goal attainment
- [Basics of Optimization]({% link airfoil_optimization/basics.md %})
  - how initialization, objective evaluation, and PSO flow work together
- [Geometry of an Airfoil]({% link airfoil_optimization/geometry.md %})
  - geometric and curvature constraints, paneling, normalization, and practical interpretation
- [Shape Functions]({% link airfoil_optimization/shape_functions.md %})
  - behavior and use cases for `bezier`, `hicks-henne`, and `bspline`
- [Worker Tool]({% link worker/worker.md %})
  - parameters used by worker actions such as `polar`, `bezier`, `bspline`, and `norm`

If you are new to Xoptfoil2, start with [Getting Started]({% link getting_started/getting_started.md %}) and then return to this chapter as a parameter lookup.