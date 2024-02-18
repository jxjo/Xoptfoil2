! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2022-2024 Jochen Guenzel

module optimization

! Entry for optimization - starts either PSO, Generic ... 

  use os_util
  use print_util
  use airfoil_operations, only : airfoil_type

  use xfoil_driver,       only : xfoil_init, xfoil_cleanup
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

    use omp_lib    

    use particle_swarm,     only : particleswarm
    use genetic_algorithm,  only : geneticalgorithm
    use simplex_search,     only : simplexsearch

    use eval_commons,       only : eval_spec_type 

    use eval,               only : set_eval_spec
    use eval,               only : get_ndv_of_flaps, get_dv0_of_flaps 
    use eval,               only : get_dv_initial_perturb_of_flaps
    use eval,               only : eval_seed_scale_objectives  
    use eval,               only : objective_function, OBJ_GEO_FAIL
    use eval,               only : write_final_results
    use eval_constraints,   only : violation_stats_print

    use shape_airfoil,      only : get_dv0_of_shape, get_ndv_of_shape
    use shape_airfoil,      only : get_dv_initial_perturb_of_shape
    use shape_airfoil,      only : set_shape_spec

    type (airfoil_type), intent(in)       :: seed_foil
    type (eval_spec_type), intent(in)     :: eval_spec
    type (shape_spec_type), intent(in)    :: shape_spec
    type (optimize_spec_type), intent(in) :: optimize_options
    type (airfoil_type), intent(out)      :: final_foil


    double precision, allocatable :: dv_final (:)
    double precision, allocatable :: dv_0 (:), dv_initial_perturb (:) 
    double precision              :: f0_ref, fmin
    integer                       :: steps, fevals, designcounter, max_threads
    integer                       :: ndv_shape, ndv, ndv_flap

    ! --- activate multithreading, thread private xfoil----------------------------------
    
    ! macro OPENMP is set in CMakeLists.txt as _OPENMP is not set by default 

    max_threads = 1                                     ! dummy for linter
#ifdef OPENMP 

    max_threads = omp_get_max_threads()        
    if (max_threads >= 4) max_threads = max_threads-1
    call omp_set_num_threads(max_threads)                   
    call print_note (stri(max_threads)//" CPU threads will be used for xfoil multi threading", 3)                  

    !$omp parallel default(shared)
    call xfoil_init()                    ! Allocate private memory for xfoil on each thread 
    !$omp end parallel
        
#endif


    ! --- initialize evaluation of airfoil ----------------------------------

    ! load evaluation and shape specification into eval module 
    ! (will be static (shared), private there during optimization) 

    call set_shape_spec (seed_foil, shape_spec)     ! seed airfoil & shape specs eg hicks-henne into shape module 
    call set_eval_spec  (eval_spec)                 ! eval specs eg op points into eval module  

    ! now evaluate seed_foil to scale objectives to objective function = 1.0 

    call eval_seed_scale_objectives (seed_foil)


    ! --- initialize solution space  -----------------------------------------
    !
    ! Solution space is defined by ndv design variables, which are normed to 0..1
    ! 
    ! Mapping of design variables to the domain specific values is done in 
    ! the 'shape modules' like 'hicks_henne' 
    ! design variables for 'shaping'

    ndv_shape = get_ndv_of_shape ()                      ! design variables of aero optimization
    ndv_flap  = get_ndv_of_flaps ()                      ! design variables of flap optimization

    ndv = ndv_shape + ndv_flap 

    allocate (dv_final(ndv))                            ! final, optimized design 

    ! Set design #0 = seed airfoil 

    allocate (dv_0(ndv))                                 
    dv_0 (1:ndv_shape)  = get_dv0_of_shape ()
    dv_0 (ndv_shape+1:) = get_dv0_of_flaps ()  

    ! get initial pertrbs of dv for initial design an initial velocity 

    allocate (dv_initial_perturb (ndv))                                 
    dv_initial_perturb (1:ndv_shape)  = get_dv_initial_perturb_of_shape ()

    ! Sanity check - eval objective dv_0 (seed airfoil) - should be 1.0

    f0_ref = objective_function (dv_0)
    
    if (f0_ref == OBJ_GEO_FAIL) then 
      call violation_stats_print (5)
      call my_stop ("Seed airfoil failed due to geometry violations. This should not happen ...")
    else if (strf('(F6.4)', f0_ref) /= strf('(F6.4)', 1d0)) then 
      call print_warning ("Objective function of seed airfoil is "//strf('(F8.6)', f0_ref)//&
                          " (should be 1.0). This should not happen ...", 5)
    end if  


    ! --- do optimization  -----------------------------------------

    steps  = 0
    fevals = 0
    designcounter = 0

    if (optimize_options%type == PSO) then

      call particleswarm (dv_0, dv_initial_perturb, optimize_options%pso_options, &
                          objective_function, &
                          dv_final, fmin, steps, fevals, designcounter)

    else if (optimize_options%type == GENETIC) then

      ! #todo remove xmin, xmax in genetic
      ! call geneticalgorithm(dv_final, fmin, steps, fevals, objective_function, &
      !                       dv_0, &
      !                       .true., f0_ref, constrained_dvs, ga_options,               &
      !                       designcounter)

    end if


    ! final evaluation and output of results 

    call write_final_results (dv_final, steps, fevals, fmin, final_foil)


    ! --- shut down multi threading, xfoil  -----------------------------------------

    !$omp parallel default(shared)
    call xfoil_cleanup()
    !$omp end parallel


  end subroutine optimize


end module optimization
