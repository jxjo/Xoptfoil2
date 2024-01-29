! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2024 Jochen Guenzel 


program main

!         Main program for airfoil optimization
!                 
!                 Modules Hirarchy 
!
!                  main   / worker
!  input_sanity    input_read   optimization_driver
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

  use input_read,           only : read_inputs
  use input_sanity,         only : check_and_process_inputs

  use eval_commons,         only : eval_spec_type

  use airfoil_operations,   only : get_seed_airfoil, te_gap
  use airfoil_operations,   only : repanel_and_normalize, repanel_bezier, make_symmetrical
  use airfoil_operations,   only : airfoil_write
  use airfoil_preparation,  only : check_seed
  use airfoil_preparation,  only : preset_airfoil_to_targets
  use airfoil_preparation,  only : transform_to_bezier_based 
  ! use airfoil_preparation,  only : matchfoils_preprocessing

  use optimization_driver,  only : optimize, optimize_spec_type
  use particle_swarm,       only : pso_options_type
  use genetic_algorithm,    only : ga_options_type
  use simplex_search,       only : simplex_options_type
  use optimization_util,    only : reset_run_control, delete_run_control

  use shape_airfoil,        only : shape_spec_type
  use shape_airfoil,        only : BEZIER
  use shape_bezier,         only : ncp_to_ndv

  use xfoil_driver,         only : xfoil_init, xfoil_cleanup

  use main_util,            only : write_final_foil

  implicit none

#ifndef PACKAGE_VERSION
#define PACKAGE_VERSION ""
#endif

  type (airfoil_type)           :: original_foil, final_foil, seed_foil
  type (optimize_spec_type)     :: optimize_options 
  type (eval_spec_type)         :: eval_spec
  type (shape_spec_type)        :: shape_spec
  
  character(:), allocatable     :: airfoil_filename, seed_airfoil_type


  !-------------------------------------------------------------------------------
  
  write(*,'(A)')
  call print_colored (COLOR_FEATURE,' Xoptfoil2')

  write(*,'(A)') '             The Airfoil Optimizer            v'//trim(PACKAGE_VERSION)
  write(*,'(A)') 

  ! Handle multithreading - be careful with screen output in multi-threaded code parts
  !   macro OPENMP is set in CMakeLists.txt as _OPENMP is not set by default 
  call set_number_of_threads()


  ! Create subdirectory for all the design files, clean existing files 

  design_subdir = output_prefix // DESIGN_SUBDIR_POSTFIX // '/'
  call remove_directory (design_subdir)

  call delete_file (output_prefix//'.dat')              ! the final airfoil 
  call delete_file (output_prefix//'.hicks')            ! ... could have been hicks henne
  call delete_file (output_prefix//'.bez')              ! ... could have been bezier 


  ! Allocate private memory for xfoil on each thread 
  ! .. quite early, as xfoil is needed for airfoil geo routines 

!$omp parallel default(shared)
  call xfoil_init()
!$omp end parallel
  

! Read inputs from namelist file

  call read_inputs ('', airfoil_filename, seed_airfoil_type, &
                    eval_spec, shape_spec, optimize_options) 
  print * 
  call check_and_process_inputs (eval_spec, shape_spec, optimize_options)
  
  
  ! Delete existing run_control file and rewrite it - most possible errors should be passed

  call reset_run_control()


  ! Load seed airfoil, repanel, normalize (if not bezier based), write as reference  

  call get_seed_airfoil (seed_airfoil_type, airfoil_filename, original_foil)

  if (original_foil%is_bezier_based) then 
    call repanel_bezier        (original_foil, eval_spec%xfoil_geom_options%npan, seed_foil)
  else
    call repanel_and_normalize (original_foil, eval_spec%xfoil_geom_options, seed_foil) 
  end if

  if (eval_spec%geo_constraints%symmetrical)  call make_symmetrical (seed_foil)


  ! Prepare Airfoil based on optimization shape type  

  if (shape_spec%type == BEZIER) then 

    if (seed_foil%is_bezier_based) then 

        ! ignore 'bezier_options' - take seed bezier definition  
        shape_spec%bezier%ncp_top = size(seed_foil%top_bezier%px)
        shape_spec%bezier%ncp_bot = size(seed_foil%bot_bezier%px)
        shape_spec%bezier%ndv     = ncp_to_ndv (shape_spec%bezier%ncp_top, shape_spec%bezier%ncp_bot)

        write(*,*)  
        call print_note ("Using number of Bezier control points from seed airfoil. "// &
                        "Values in 'bezier_options' will be ignored.")
        call print_text ("Also no preprocessing of seed airfoil will be done.", 7)

    else

      ! a new bezier "match foil" is generated to be new seed 
      seed_foil%name = seed_foil%name // '_bezier'

      call transform_to_bezier_based (shape_spec%bezier, seed_foil%npoint, seed_foil)

    end if 
  end if  


  ! write final seed airfoil as reference 

  call airfoil_write (seed_foil%name//'.dat', seed_foil%name, seed_foil)             

  ! Set up for matching airfoils 

  if (eval_spec%match_foils) then
    call matchfoils_preprocessing  (seed_foil, eval_spec%foil_to_match_name)
  end if

  ! Make sure seed airfoil passes constraints - final checks, prepare objective function 
  !  - get scaling factors for operating points with xfoil, 

  call check_seed (seed_foil, shape_spec, eval_spec%curv_constraints, eval_spec%geo_constraints, &
                   eval_spec%xfoil_options)

  ! Prepare out put directory 

  call make_directory (design_subdir)


  ! Optimize
  
  call optimize (seed_foil, eval_spec, shape_spec, optimize_options, final_foil) 


  ! Write airfoil to file

  final_foil%name   = output_prefix
  call write_final_foil (final_foil) 


  ! clean up 

  call delete_run_control()

!$omp parallel default(shared)
  call xfoil_cleanup()
!$omp end parallel


end program main

