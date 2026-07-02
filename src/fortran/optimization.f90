! MIT License

module optimization

! Entry for optimization - starts either PSO, Generic ... 

  use os_util
  use print_util
  use string_util,        only : stri, strf
  use airfoil_base,       only : airfoil_type

  use xfoil_driver,       only : xfoil_init, xfoil_cleanup
  use particle_swarm,     only : pso_options_type
  use simplex_search,     only : simplex_options_type
  use shape_airfoil,      only : shape_spec_type, BEZIER, HICKS_HENNE

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
    integer                       :: cpu_threads          ! number of threads for multi threading 
    type(pso_options_type)        :: pso_options
    type(simplex_options_type)    :: sx_options
  end type optimize_spec_type

 
  ! --------- private --------------------------------------------------------

  contains


  subroutine optimize (seed_foil, eval_spec, optimize_options, &
                       final_foil, final_flap_angles)

    !----------------------------------------------------------------------------
    !! main optimization controller 
    !----------------------------------------------------------------------------

    use omp_lib    
    use math_util,          only : round

    use particle_swarm,     only : particleswarm

    use eval_commons,       only : eval_spec_type 

    use eval_setup,         only : adjust_weightings, eval_seed_scale_objectives
    use eval,               only : init_eval_state
    use eval,               only : objective_function, OBJ_DESIGN_FAIL, OBJ_XFOIL_FAIL
    use eval,               only : write_final_results
    use eval_constraints,   only : penalty_stats_init, print_penalty_stats_table

    use shape_airfoil,      only : get_dv0_of_shape, get_ndv_of_shape, get_dv_initial_perturb_of_shape
    use shape_airfoil,      only : get_ndv_of_flaps, get_dv0_of_flaps, get_dv_initial_perturb_of_flaps 
    use shape_airfoil,      only : set_shape_spec

    type (airfoil_type), intent(in)             :: seed_foil
    type (eval_spec_type), intent(inout)        :: eval_spec
    type (optimize_spec_type), intent(in)       :: optimize_options
    type (airfoil_type), intent(out)            :: final_foil
    double precision, allocatable, intent(out)  :: final_flap_angles (:)


    double precision, allocatable :: dv_final (:)
    double precision, allocatable :: dv_0 (:), dv_initial_perturb (:) 
    double precision              :: elapsed_seconds, f_seed, f_best
    integer                       :: steps, fevals
    integer                       :: ndv_shape, ndv, ndv_flap
    integer                       :: itime_start
    integer                       :: threads_available, threads

    ! --- activate multithreading, thread private xfoil----------------------------------
    
    ! macro OPENMP is set in CMakeLists.txt as _OPENMP is not set by default 

    call system_clock(count=itime_start)

    threads_available = 1                                     ! dummy for linter
    threads = 1
#ifdef OPENMP 
    threads_available = omp_get_max_threads()  
    if (optimize_options%cpu_threads > 0) then 
      threads = optimize_options%cpu_threads
    else
      threads = threads_available - abs(optimize_options%cpu_threads)
    end if 
    threads = min (max (1, threads) , threads_available) 
    call omp_set_num_threads(threads)                   
    call print_note ("Optimization will use "//stri(threads)//" of "//stri(threads_available)//" CPU threads", 3)                  

    !$omp parallel default(shared)
    call xfoil_init()                    ! Allocate private memory for xfoil on each thread 
    !$omp end parallel      
#else
    call xfoil_init()                    ! Serial/debug build still needs Xfoil workspace allocated
#endif


    ! --- initialize evaluation of airfoil ----------------------------------

    call adjust_weightings(eval_spec)
    call eval_seed_scale_objectives(seed_foil, eval_spec)

    ! load evaluation specification into eval module
    ! (will be static (shared), private there during optimization)

    call init_eval_state(eval_spec)


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

    ! get initial perturbs of dv for initial design an initial velocity 

    allocate (dv_initial_perturb (ndv))                                 
    dv_initial_perturb (1:ndv_shape)  = get_dv_initial_perturb_of_shape ()
    dv_initial_perturb (ndv_shape+1:) = get_dv_initial_perturb_of_flaps ()

    ! reset statistics of geometry violations before optimization 
    call penalty_stats_init ()

    ! Evaluate objective dv_0 (seed airfoil).

    f_seed = objective_function (dv_0)  
    
    if (round(f_seed, 6) == OBJ_DESIGN_FAIL .or. round(f_seed, 6) == OBJ_XFOIL_FAIL) then 
      print *, f_seed
      call print_warning ("Objective function of seed airfoil is "//strf('F8.5', f_seed)//&
                          " (invalid during setup).", 5)
    end if  


    ! --- do optimization  -----------------------------------------

    steps  = 0
    fevals = 0

    if (optimize_options%type == PSO) then

      call particleswarm (dv_0, dv_initial_perturb, f_seed, optimize_options%pso_options, &
              objective_function, dv_final, f_best, steps, fevals)
                          
    else 
      call my_stop ("Unknown optimization type: "//stri(optimize_options%type))
    end if


    ! final evaluation and output of results 

    elapsed_seconds = elapsed_s(itime_start)

    call write_final_results (dv_final, f_seed, f_best, elapsed_seconds, final_foil, final_flap_angles) 

    ! --- shut down multi threading, xfoil  -----------------------------------------

    !$omp parallel default(shared)
    call xfoil_cleanup()
    !$omp end parallel

  end subroutine optimize


end module optimization
