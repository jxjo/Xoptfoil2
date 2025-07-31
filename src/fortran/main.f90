! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2020-2025 Jochen Guenzel 


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
!   optimization_util          polar_operations
!                 
!                      eval 
!              eval_out  eval_constraints 
!                  eval_commons  
! 
!                 shape_airfoil     
!  airfoil_geometry            xfoil_driver
!                 airfoil_base 
!
!  shape_bezier  shape_hicks_henne  shape_camb_thick 
!            spline simplex_search 
!
!                print_util
!      os_util  math_util  commons  xfoil_inc
!

  use os_util
  use commons
  use print_util 

  use airfoil_base,         only : airfoil_type
  use airfoil_base,         only : airfoil_write_with_shapes
  use airfoil_preparation,  only : prepare_seed_foil, prepare_match_foil

  use input_read,           only : read_inputs, run_mode_from_command_line
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
  logical                       :: wait_at_end

  !-------------------------------------------------------------------------------
   
  print *
  call print_colored (COLOR_FEATURE,' '//PGM_NAME)
  print *,'             The Airfoil Optimizer             '//trim(PACKAGE_VERSION)
  print *

  ! multithreading will be activated in 'optimize' with xfoil initialization 
  !omp single

  ! get run_mode from command 

  if (run_mode_from_command_line () == MODE_CHILD_PROCESS) then 
    call set_my_stop_to_stderr (.true.) 
  end if 

  ! Read inputs from namelist file

  call read_inputs ('', airfoil_filename, output_prefix, show_details, wait_at_end, &
                    eval_spec, shape_spec, optimize_options) 

  call check_and_process_inputs (eval_spec, shape_spec, optimize_options)
  
  
  ! create design directory for outputs during optimization 

  design_subdir = output_prefix // DESIGN_SUBDIR_POSTFIX // '/'
  call make_directory (design_subdir)
  call reset_run_control()
  

  ! Load seed airfoil, repanel, normalize (if not bezier based), write as reference  

  call print_header ("Preparing seed airfoil")
  call prepare_seed_foil (airfoil_filename, eval_spec, shape_spec, seed_foil)

  ! prepare match-foil
  
  if (eval_spec%match_foil_spec%active) then
    call print_header ("Preparing match airfoil")
    call prepare_match_foil (seed_foil, eval_spec%match_foil_spec) 
  end if 
  
  ! Have a look at the shaping paramters   

  call print_header ("Assessment of shape functions")

  call set_shape_spec (seed_foil, shape_spec)           ! seed airfoil & shape specs eg hicks-henne into shape module 
  call assess_shape ()

  ! delete old airfoil result (quite late so input errors won't remove last result )

  call delete_file (output_prefix//'.dat')              ! delete the final airfoil
  call delete_file (output_prefix//'_f*.dat')           ! ... and maybe flapped versions
  call delete_file (output_prefix//'.hicks')            ! ... could have been hicks henne
  call delete_file (output_prefix//'.bez')              ! ... could have been bezier 

  ! Optimize
  
  call print_header ("Initializing optimization")

  call optimize (seed_foil, eval_spec, optimize_options, final_foil, final_flap_angles) 


  ! Write airfoil to file

  final_foil%name   = output_prefix
  Call set_show_details (.true.)                        ! ensure print of final airfoil 
  call airfoil_write_with_shapes (final_foil, "", highlight=.true.) 

  ! Write flapped versions of final airfoil 

  if (shape_spec%flap_spec%use_flap) then 
    call delete_file (output_prefix//'_f*.dat')           ! ... and maybe old flapped versions exist
    call write_airfoil_flapped (final_foil, shape_spec%flap_spec, final_flap_angles, .true.) 
  end if 
  
  ! clean up - optional: wait at end for user to press enter

  call delete_run_control()
  print *

  if (wait_at_end) then 
    call print_note ("Press Enter to finish ... ", 1, no_crlf=.true.)
    read (*,*)  
  end if 

end program main

