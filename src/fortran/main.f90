! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2020-2024 Jochen Guenzel 


program main

! Main program for airfoil optimization


! Xoptfoil2 main building blocks 
! ------------------------------
!     
!    - main           main controller  
!    - input          process inputs, sanity checks 
!    - preparation    prepare seed airfoil for optimization 
!    - optimize       iniitalize, main controller, PSO
!    - evaluation     objective function, aero and geo properties 
!    - shape          create new shape out of design variables 
!
!            
! Xoptfoil2 modules hirarchy
! --------------------------
!
!
!                  main   / worker
!
!           input_sanity    input_read   
!                  
!     optimization             airfoil_preparation
!    particle_swarm  
!   optimization_util     
!                 
!                      eval 
!              eval_out  eval_constraints 
!                  eval_commons  
! 
!                 shape_airfoil     polar_operations
!  airfoil_geometry            xfoil_driver
!                 airfoil_base 
!
!  shape_bezier  shape_hicks_henne  shape_camb_thick 
!              spline simplex_search 
!
!      os_util  math_util  commons  xfoil_inc
!

  use os_util
  use commons
  use print_util 

  use airfoil_base,         only : airfoil_type
  use airfoil_base,         only : airfoil_write_with_shapes
  use airfoil_preparation,  only : prepare_seed

  use input_read,           only : read_inputs
  use input_sanity,         only : check_and_process_inputs

  use eval_commons,         only : eval_spec_type
  use eval_out,             only : write_airfoil_flapped

  use shape_airfoil,        only : shape_spec_type
  use shape_airfoil,        only : set_shape_spec, assess_shape

  use optimization,         only : optimize, optimize_spec_type
  use optimization_util,    only : reset_run_control, delete_run_control


  implicit none

#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION ""
#endif

  type (airfoil_type)           :: final_foil, seed_foil
  type (optimize_spec_type)     :: optimize_options 
  type (eval_spec_type)         :: eval_spec
  type (shape_spec_type)        :: shape_spec
  
  character(:), allocatable     :: airfoil_filename
  double precision, allocatable :: final_flap_angles (:) 


  !-------------------------------------------------------------------------------
  
  print *
  call print_colored (COLOR_FEATURE,' Xoptfoil2')
  print *,'             The Airfoil Optimizer             '//trim(PACKAGE_VERSION)
  print *

  ! multithreading will be activated in 'optimize' with xfoil initialization 
  !omp single


  ! Read inputs from namelist file

  call read_inputs ('', airfoil_filename, output_prefix, show_details, &
                    eval_spec, shape_spec, optimize_options) 

  call check_and_process_inputs (eval_spec, shape_spec, optimize_options)
  
  
  ! Delete existing run_control file and rewrite it - most possible errors should be passed

  design_subdir = output_prefix // DESIGN_SUBDIR_POSTFIX // '/'
  call make_directory (design_subdir)
  call delete_file (output_prefix//'.dat')              ! the final airfoil 
  call delete_file (output_prefix//'.hicks')            ! ... could have been hicks henne
  call delete_file (output_prefix//'.bez')              ! ... could have been bezier 
  call reset_run_control()
  

  ! Load seed airfoil, repanel, normalize (if not bezier based), write as reference  

  call print_header ("Preparing seed airfoil")

  call prepare_seed (airfoil_filename, eval_spec, shape_spec, seed_foil)


  ! Have a look at the shaping paramters   

  call print_header ("Assessment of shape functions")

  call set_shape_spec (seed_foil, shape_spec)     ! seed airfoil & shape specs eg hicks-henne into shape module 
  call assess_shape ()


  ! Optimize
  
  call print_header ("Initializing optimization")

  call optimize (seed_foil, eval_spec, shape_spec, optimize_options, final_foil, final_flap_angles) 


  ! Write airfoil to file

  final_foil%name   = output_prefix
  Call set_show_details (.true.)                    ! ensure print of final airfoil 
  call airfoil_write_with_shapes (final_foil, "", highlight=.true.) 

  ! Write flapped versions of final airfoil 

  if (shape_spec%flap_spec%use_flap) then 
    call write_airfoil_flapped (final_foil, shape_spec%flap_spec, final_flap_angles, .true.) 
  end if 
  
  ! clean up 

  call delete_run_control()

  print *

end program main

