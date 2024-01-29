! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2022-2024 Jochen Guenzel

module optimization

! Entry for optimization - starts either PSO, Generic ... 

  use os_util
  use particle_swarm,     only : pso_options_type
  use genetic_algorithm,  only : ga_options_type
  use simplex_search,     only : simplex_options_type

  use shape_airfoil,      only : shape_spec_type, BEZIER, HICKS_HENNE, CAMB_THICK 



  implicit none
  private

  integer, parameter, public  :: PSO      = 1            ! optimization search types 
  integer, parameter, public  :: GENETIC  = 2
  integer, parameter, public  :: SIMPLEX  = 3

  public :: optimize 
  public :: optimize_spec_type

  ! Main specification of optimization / options 

  type optimize_spec_type   
    integer                       :: type                 ! type of optimization  
    type(pso_options_type)        :: pso_options
    type(ga_options_type)         :: ga_options
    type(simplex_options_type)    :: sx_options
  end type optimize_spec_type



  ! --------- private --------------------------------------------------------

  contains


  subroutine optimize (seed_foil, eval_spec, shape_spec, optimize_options, final_foil)

    !----------------------------------------------------------------------------
    !! optimization controller - calls either PSO or Genetic 
    !----------------------------------------------------------------------------

    use commons,            only : airfoil_type

    use particle_swarm,     only : particleswarm
    use genetic_algorithm,  only : geneticalgorithm
    use simplex_search,     only : simplexsearch

    use eval_commons,       only : eval_spec_type 

    use eval,               only : set_eval_spec 
    use eval,               only : eval_seed_scale_objectives  
    use eval,               only : objective_function, OBJ_GEO_FAIL
    use eval,               only : write_progress, write_final_results

    use shape_airfoil,      only : get_dv0_of_shape
    use shape_airfoil,      only : set_shape_spec, set_seed_foil

    type (airfoil_type), intent(in)       :: seed_foil
    type (eval_spec_type), intent(in)     :: eval_spec
    type (shape_spec_type), intent(in)    :: shape_spec
    type (optimize_spec_type), intent(in) :: optimize_options
    type (airfoil_type), intent(out)      :: final_foil


    double precision, allocatable :: dv_final (:)
    double precision, allocatable :: dv0 (:) 
    double precision              :: f0_ref, fmin
    integer                       :: steps, fevals, designcounter
    integer                       :: ndv_shape, ndv, ndv_flap
    logical                       :: initial_dv0_based


    ! --- initialize evaluation of airfoil ----------------------------------

    ! load evaluation and shape specification into modules (will be static, private there) 

    call set_seed_foil  (seed_foil) 
    call set_shape_spec (shape_spec)           ! load shape specs eg hicks-henne into shape module 
    call set_eval_spec  (eval_spec)            ! load eval specs eg op points into eval module  

    ! now evaluate seed_foil to scale objectives to objective function = 1.0 

    call eval_seed_scale_objectives (seed_foil)


    ! --- initialize optimization design variables  -----------------------

    ! design variables for 'shaping'

    if (shape_spec%type == BEZIER) then 
      ndv_shape = shape_spec%bezier%ndv
    elseif (shape_spec%type == HICKS_HENNE) then 
      ndv_shape = shape_spec%hh%ndv
    else 
      ndv_shape = shape_spec%camb_thick%ndv
    end if 

    ! design variables for flap optimization 
    ndv_flap = eval_spec%flap_spec%ndv 

    ! total number of design variables 
    ndv = ndv_shape + ndv_flap 

    allocate (dv0(ndv))
    allocate (dv_final(ndv))

    ! Set initial design = seed airfoil 

    dv0 (1:ndv_shape) = get_dv0_of_shape (shape_spec%type)

    ! #todo dv0 (ndv_shape+1:) = designvars_0_flap () 


    ! Compute objective f0_ref of seed airfoil - this should be 1.0

    f0_ref = objective_function (dv0)
    
    if (f0_ref == OBJ_GEO_FAIL) then 
      call my_stop ("Seed airfoil failed due to geometry constraints. This should not happen ...")
    end if 

    ! Write seed airfoil coordinates and polars to file

    call write_progress (dv0, 0) 

    ! Set up mins and maxes

    ! #todo - remove 
    
    if (shape_spec%type == CAMB_THICK) then

      initial_dv0_based = .false.                     ! inital designs will between 0 and 1

    elseif (shape_spec%type == BEZIER) then

      initial_dv0_based = .true.                     ! inital designs will be close to dv0

    else      ! Hicks-Henne 

      initial_dv0_based = .false.                     ! inital designs will between 0 and 1

    end if

    ! Finally - do optimization -----------

    steps  = 0
    fevals = 0
    designcounter = 0

    if (optimize_options%type == PSO) then

      call particleswarm (dv_final, fmin, steps, fevals, objective_function, &
                          dv0, initial_dv0_based, &
                          f0_ref,  &
                          optimize_options%pso_options, designcounter)

    else if (optimize_options%type == GENETIC) then

      ! #todo remove xmin, xmax in genetic
      ! call geneticalgorithm(dv_final, fmin, steps, fevals, objective_function, &
      !                       dv0, initial_dv0_based, &
      !                       .true., f0_ref, constrained_dvs, ga_options,               &
      !                       designcounter)

    end if


    ! final evaluation and output of results 

    call write_final_results (dv_final, steps, fevals, f0_ref, fmin, final_foil)


  end subroutine optimize


end module optimization
