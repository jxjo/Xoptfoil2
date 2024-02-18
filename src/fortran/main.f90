! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2024 Jochen Guenzel 


program main

!         Main program for airfoil optimization
!                 
!                 Modules Hirarchy 
!
!                  main   / worker
!  input_sanity    input_read   optimization
!                  airfoil_preparation
! particle_swarm  genectic_algorithm   simplex_search
!                 eval
!      airfoil_operations   polar_operations
!                    memory_util
!                  shape_airfoil
!       airfoil_shape_bezier optimization_util 
!                    math_util
!                   xfoil_driver
!             xfoil   os_util  commons
!

  use os_util
  use commons
  use print_util 

  use airfoil_operations,   only : airfoil_type
  use airfoil_operations,   only : airfoil_write_with_shapes
  use airfoil_preparation,  only : prepare_seed

  use input_read,           only : read_inputs
  use input_sanity,         only : check_and_process_inputs

  use eval_commons,         only : eval_spec_type

  use shape_airfoil,        only : shape_spec_type
  use shape_airfoil,        only : assess_shape

  use optimization,         only : optimize, optimize_spec_type
  use optimization_util,    only : reset_run_control, delete_run_control

  use main_util

  implicit none

#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION ""
#endif

  type (airfoil_type)           :: final_foil, seed_foil
  type (optimize_spec_type)     :: optimize_options 
  type (eval_spec_type)         :: eval_spec
  type (shape_spec_type)        :: shape_spec
  
  character(:), allocatable     :: airfoil_filename


  !-------------------------------------------------------------------------------
  
  print *
  call print_colored (COLOR_FEATURE,' Xoptfoil2')
  print *,'             The Airfoil Optimizer            v'//trim(PACKAGE_VERSION)
  print *

  ! multithreading will be activated in 'optimize' with xfoil initialization 
  !omp single


  ! Read inputs from namelist file

  call print_header ("Processing input")

  call read_inputs ('', airfoil_filename, output_prefix, show_details, &
                    eval_spec, shape_spec, optimize_options) 

  call check_and_process_inputs (eval_spec, shape_spec, optimize_options)
  
  
  ! Delete existing run_control file and rewrite it - most possible errors should be passed

  call clean_old_output ()
  call make_directory (design_subdir)
  call reset_run_control()
  

  ! Load seed airfoil, repanel, normalize (if not bezier based), write as reference  

  call print_header ("Preparing seed airfoil")

  call prepare_seed (airfoil_filename, eval_spec, shape_spec, seed_foil)


  ! Have a look at the shaping paramters   

  call print_header ("Assessment of shape functions")

  call assess_shape (shape_spec)


  ! Optimize
  
  call print_header ("Initializing optimization")

  call optimize (seed_foil, eval_spec, shape_spec, optimize_options, final_foil) 


  ! Write airfoil to file

  final_foil%name   = output_prefix
  call airfoil_write_with_shapes (final_foil, "") 

  
  ! clean up 

  call delete_run_control()

  print *

end program main

