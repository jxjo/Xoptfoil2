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
!                    math_deps
!                   xfoil_driver
!             xfoil   os_util  commons
!

  use os_util
  use commons
  use print_util 

  use input_read,           only : read_inputs
  use input_sanity,         only : check_and_process_inputs

  use eval_commons,         only : eval_spec_type
  use shape_airfoil,        only : shape_spec_type

  use airfoil_operations,   only : airfoil_write_with_shapes
  use airfoil_preparation,  only : prepare_seed_foil

  use optimization,         only : optimize, optimize_spec_type
  use optimization_util,    only : reset_run_control, delete_run_control

  use xfoil_driver,         only : xfoil_init, xfoil_cleanup

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
  
  write(*,'(A)')
  call print_colored (COLOR_FEATURE,' Xoptfoil2')

  write(*,'(A)') '             The Airfoil Optimizer            v'//trim(PACKAGE_VERSION)
  write(*,'(A)') 

  ! Handle multithreading - be careful with screen output in multi-threaded code parts
  !   macro OPENMP is set in CMakeLists.txt as _OPENMP is not set by default 
  call set_number_of_threads()


  ! Allocate private memory for xfoil on each thread 
  ! .. quite early, as xfoil is needed for airfoil geo routines 

  !$omp parallel default(shared)
  call xfoil_init()
  !$omp end parallel
  

  ! Read inputs from namelist file

  call print_header ("Processing input")

  call read_inputs ('', airfoil_filename, output_prefix, show_details, &
                    eval_spec, shape_spec, optimize_options) 

  call check_and_process_inputs (eval_spec, shape_spec, optimize_options)
  
  
  ! Delete existing run_control file and rewrite it - most possible errors should be passed

  call clean_old_output ()
  call reset_run_control()


  ! Load seed airfoil, repanel, normalize (if not bezier based), write as reference  

  call print_header ("Preparing seed airfoil")

  call prepare_seed_foil (airfoil_filename, eval_spec, shape_spec, seed_foil)


  ! Optimize
  
  call print_header ("Initializing optimization")

  call make_directory (design_subdir)

  call optimize (seed_foil, eval_spec, shape_spec, optimize_options, final_foil) 


  ! Write airfoil to file

  final_foil%name   = output_prefix
  call airfoil_write_with_shapes (final_foil) 

  
  ! clean up 

  call delete_run_control()

  !$omp parallel default(shared)
  call xfoil_cleanup()
  !$omp end parallel

  print *


end program main

