! MIT License

module particle_swarm

  use os_util 
  use print_util
  use string_util,       only : stri, strf

  ! Particle Swarm Optimization - PSO  


  implicit none

  type pso_options_type                 ! Options type definition for PSO

    integer :: pop                      ! particle swarm population size
    double precision :: min_radius      ! min radius of designs before
                                        !   triggering a stop condition
    double precision :: max_speed       ! Max speed allowed for particles
    integer :: max_iterations           ! Max iterations allowed before stopping
    character(:),allocatable :: convergence_profile
                                        ! 'exhaustive' or 'quick'

    integer :: max_retries = 3          ! max. number of retries a single 
                                        ! particle tries to get a valid geometry

    logical :: dump_dv = .false.        ! dump design variables to file 

  end type pso_options_type

  contains 


  subroutine particleswarm (dv_0, dv_initial_perturb, f_seed, pso_options, & 
                            objfunc, dv_opt, f_best, iteration, fevals)

    !----------------------------------------------------------------------------
    !! Particle swarm optimization routine. 
    !! dv_0:    design variables of design 0 which should be seed airfoil 
    !----------------------------------------------------------------------------
                          
    use commons,              only : design_subdir, show_details
    use optimization_util,    only : init_random_seed, initial_designs,             &
                                     design_radius, dump_design, clip_velocity, initial_velocities 
    use optimization_util,    only : reset_run_control, stop_requested, update_run_control
    use optimization_util,    only : write_history_header, write_history
    use optimization_util,    only : debug_print_f_values, debug_print_retry

    use eval,                 only : write_progress, eval_design_is_valid
    use shape_airfoil,        only : debug_print_vel_as_shape_data, debug_print_dv_as_shape_data

    double precision, intent(in)        :: dv_0 (:), dv_initial_perturb (:)
    double precision, intent(in)        :: f_seed
    type (pso_options_type), intent(in) :: pso_options
    double precision, intent(inout)     :: dv_opt (:)

    interface
      double precision function objfunc(v)
        double precision, intent(in)  :: v (:)
      end function
    end interface
    
    double precision, intent(out) :: f_best
    integer, intent(out)          :: iteration, fevals

    double precision, dimension(pso_options%pop)  :: f_values, f_personal_best
    double precision, dimension(size(dv_0,1))     :: c1_random, c2_random, new_vel
    double precision, dimension(size(dv_0,1),pso_options%pop) :: dv, vel, best_design
    double precision              :: c1, c2, whigh, wlow, convrate, max_speed, wcurr, f_iteration_best, &
                                     radius, speed_reduction
    logical                       :: finished, improved, converged, max_reached, targets_achieved, is_valid
    character(:), allocatable     :: histfile
    integer                       :: i, i_min, ndv, i_retry, ndesigns, ndone, ndone_local
    

    call print_header ('Particle swarm with '//stri(pso_options%pop)// ' members will now try its best ...')


    ! PSO tuning variables

    if (pso_options%convergence_profile == "quick") then

      c1 = 0.8d0 !0.7d0                      ! particle-best trust factor
      c2 = 1.30! 1.4d0                      ! swarm-best trust factor
      whigh = 1.0d0!0.9d0                   ! starting inertial parameter
      wlow = 0.4d0!0.3d0                    ! ending inertial parameter
      convrate = 0.06d0               ! inertial parameter reduction rate

    else if (pso_options%convergence_profile == "exhaustive") then

      c1 = 1.3d0                       ! particle-best trust factor - higher than c2 to maintain diversity
      c2 = 1.1d0                       ! swarm-best trust factor
      whigh = 1.6d0                    ! starting inertial parameter
      wlow = 0.5d0                     ! ending inertial parameter - lower than quick(0.6) but not too sticky
      convrate = 0.03d0                ! inertial parameter reduction rate - drops below 1.0 at ~iter 19

    else
      call my_stop ("Unknown convergence_profile: "// pso_options%convergence_profile)
    end if
      
    wcurr     = whigh                           ! initial Inertial parameter
    max_speed = pso_options%max_speed           ! speed limit = initial_perturb  
    ndv       = size(dv,1)

    call init_random_seed()                     ! init Fortran random seeds 


    ! --- Set up initial designs and their obj values

    call initial_designs (dv_0, dv_initial_perturb, f_seed, dv, f_values)

    ! Initial velocities 
    
    vel = initial_velocities (dv, max_speed)

    ! Global and local best so far

    best_design     = dv                              ! Matrix of best designs for each particle
    f_personal_best = f_values                        ! vector of each particle's best objective so far

    f_best      = f_seed                              ! particle #1 is dv0
    dv_opt      = dv(:,1)                             ! best design so far is dv0 

    ! Counters
    
    fevals        = 0                                 ! initial evaluation count
    iteration     = 0
    ndesigns      = 0

    ! Finish conditions 

    converged        = .false.
    targets_achieved = .false.
    finished         = .false.
    max_reached      = .false.

    ! Open file for writing iteration history

    histfile  = design_subdir//'Optimization_History.csv'
    call write_history_header (histfile) 
    call write_history        (histfile, iteration, .false., ndesigns, design_radius(dv), f_best, f_seed)

    ! Write seed airfoil coordinates and polars to file
    call write_progress (dv_0, 0) 
    
    ! init run control with design #0 info 
    call update_run_control (0, 0, f_best)


    ! --- Begin optimization


    call show_optimization_header  (pso_options, show_details)

    !$omp parallel default(shared) private(i, i_retry, is_valid, ndone_local) 

    do while (.not. finished)

      !$omp master
      ndone = 0
      iteration = iteration + 1

      call show_iteration_number (pso_options%max_iterations, iteration, pso_options%max_retries)
      !$omp end master
      !$omp barrier

      ! Use OMP DYNAMIC (and not STATIC which is default) so every thread will take a new "i" 
      ! when it finished its task. In summary this is much faster as calculation time differs
      ! very much depending if a xfoil calculation will be made or not (geo constraints!)     

      !$omp do SCHEDULE(DYNAMIC)

      ! Update each particle's position, evaluate objective function, etc.

      do i = 1, pso_options%pop

        dv(:,i)    = dv(:,i) + vel(:,i)               ! update position

        ! call print_dv_as_shape_data (i, dv(:,i))      ! dump design variables as shape data for debugging

        f_values(i) = objfunc(dv(:,i))                ! evaluate objective at new position

        !$omp atomic capture
        ndone = ndone + 1
        ndone_local = ndone
        if (show_details) call show_particles_progress (pso_options%pop, ndone_local)
       
      end do   

      !$omp end do

      !$omp barrier
      !$omp master

      ! result evaluation  and particles update --> single threaded

      call show_particles_info (f_best, f_personal_best, f_values)

      ! Update best overall 

      f_iteration_best = minval(f_values,1)
      if (f_iteration_best < f_best) then
        f_best    = f_iteration_best
        i_min     = minloc(f_values,1)
        dv_opt    = dv(:,i_min)
        improved  = .true.
        ndesigns  = ndesigns + 1
      else
        improved  = .false.
      end if

      radius = design_radius(dv)

      ! Display result of iteration

      call show_iteration_result (pso_options%min_radius, radius, f_best, f_seed, ndesigns, improved)

      ! Update velocity of each particle

      do i = 1, pso_options%pop

        ! Update  best design if appropriate

        if (f_values(i) < f_personal_best(i)) then
          f_personal_best(i) = f_values(i)
          best_design(:,i) = dv(:,i)
        end if

        speed_reduction = 1.0d0
        i_retry = 0

        do 

          ! The incredible Particle Swarm formula with new random factors each retry

          call random_number(c1_random)
          call random_number(c2_random)

          new_vel = wcurr * vel(:,i) + &                                    ! inertia
                    c1 * c1_random * (best_design(:,i) - dv(:,i)) +   &     ! particle best
                    c2 * c2_random * (dv_opt - dv(:,i))                     ! swarm best

          new_vel = speed_reduction * new_vel                                   ! gentle reduction for retries
          new_vel = clip_velocity (dv(:,i), max_speed, new_vel)             ! ensure maxspeed and boundary respect

          call eval_design_is_valid (dv(:,i) + new_vel, is_valid)           ! check if new design is valid

          if (is_valid .or. i_retry >= pso_options%max_retries ) then 
            exit
          end if

          i_retry = i_retry + 1
          speed_reduction = speed_reduction * 0.8d0   ! if not valid, reduce speed more for next retry

        end do 

        vel(:,i) = new_vel

        ! call debug_print_retry (i, i_retry, speed_reduction, new_vel)
        ! call print_vel_as_shape_data (i, vel(:,i))   


      end do

      ! call debug_print_f_values (f_values, f_personal_best, f_best, improved)

      ! Reduce inertial parameter

      wcurr = wcurr - convrate*(wcurr - wlow)

      !$omp end master
      !$omp barrier                             

      ! Write design to file, update run control, write history - all I/O in one block 

      !$omp single  
      if (improved) then
        call write_progress (dv_opt, ndesigns, targets_achieved)
      end if
      call update_run_control (iteration, ndesigns, f_best)
      call write_history (histfile, iteration, improved, ndesigns, radius, f_best, f_seed)

      ! do we finish?

      converged   = (radius < pso_options%min_radius)         ! all particles close together
      max_reached = (iteration >= pso_options%max_iterations) ! max iterations reached

      finished = converged .or. stop_requested() .or. max_reached .or. targets_achieved

      !$omp end single nowait

      ! let all particles start together a new round 
      !$omp barrier                             

    end do 

    !$omp end parallel

    if (stop_requested()) then
      print *
      call print_colored (COLOR_WARNING, '   Stop command')
      call print_text ('encountered in run_control')
    else if (max_reached) then
      print * 
      call print_colored (COLOR_WARNING, '   Max number')
      call print_text ('of iterations reached')
    else if (targets_achieved) then 
      print * 
      call print_colored (COLOR_GOOD, '   All targets achieved')
      print *
    else if (converged) then
      print * 
      call print_colored (COLOR_NORMAL, '   Convergence achieved.')
      call print_text ('All particles within radius '//strf('ES9.1',pso_options%min_radius))
      print *
    end if

        
    ! Calculate number of function evaluations
    fevals = fevals + iteration*pso_options%pop         ! this is not correct as we have retries 

  end subroutine particleswarm



  subroutine  show_optimization_header  (pso_options, show_details)

    !! Shows user info - header of optimization out 

    type (pso_options_type), intent(in) :: pso_options
    logical, intent(in)                 :: show_details

    integer                       :: retr, prog_width
    character(:), allocatable     :: retry_string

    retr = pso_options%max_retries
    if (retr > 0 ) then 
      if (retr > 1) then 
        retry_string = "retries" 
      else 
        retry_string = "retry" 
      end if
      call print_note (stri(retr)//" "//retry_string//" of particle having failed geometry.", 3)
    end if

    print *
    call print_text    ("Particle result:  '", 3, no_crlf=.true.)
    call print_colored (COLOR_GOOD,  "+")
    call print_colored (COLOR_NOTE,  "' new swarm best  '")
    call print_colored (COLOR_NOTE,  "+")
    call print_colored (COLOR_NOTE,  "' personal best   '")
    call print_colored (COLOR_NOTE,  "-")
    call print_colored (COLOR_NOTE,  "' no improv   '")
    call print_colored (COLOR_NOTE,  ".")
    call print_colored (COLOR_NOTE,  "' design fail  '")
    call print_colored (COLOR_ERROR, "x")
    call print_colored (COLOR_NOTE,  "' xfoil fail")
    print *
    print *

    ! column header: widths must match show_iteration_number + show_particles_info output
    !   Iterat: stri(iter,8)+':' = 9,  retries: ' r#' = 3  → 12 total
    !   Progress: pop / (pop/10) dots  (= 10 for pop >= 20)
    !   Particles: 1 leading space + pop chars = pop+1
    !   Radius: ES9.1 field = 9 chars (leading spaces preserved)
    prog_width = pso_options%pop / max(1, pso_options%pop / 10)
    call print_text  ("Iterat   ", 3, no_crlf=.true.)
    if (show_details) call print_fixed ('Progress', prog_width)
    call print_fixed (' Particles result', pso_options%pop + 1)
    call print_fixed ('  Radius', 9)
    call print_colored (COLOR_NOTE, '    Improvement')
    print *

  end subroutine show_optimization_header



  subroutine  show_iteration_number (max_iter, iteration, max_retries)

    !! print iteration number an number of retries 

    integer, intent(in)          :: max_iter, iteration, max_retries

    if (iteration < max_iter) then 
      call print_colored (COLOR_NORMAL, stri(iteration,8) // ':')
    else 
      call print_colored (COLOR_WARNING, stri(iteration,8) // ':')
    end if 

    if (max_retries == 0) then 
      call print_colored (COLOR_NOTE,  '   ')
    else
      call print_colored (COLOR_NOTE,  ' r'//stri(max_retries))
    end if

  end subroutine



  subroutine  show_iteration_result (min_radius, radius, f_best, f_seed, designcounter, improved)

    !! Shows user info about result of a single iteration 

    double precision, intent(in)  :: min_radius, radius, f_best, f_seed
    logical, intent(in)           :: improved
    integer, intent(in)           :: designcounter
    character(25)                 :: outstring
    double precision, parameter   :: EPSILON = 1d-12

    outstring = strf('ES9.1',radius, fix=.true.)
    if (radius < min_radius) then
      call  print_colored (COLOR_FEATURE, trim(outstring))
    else
      if (improved) then 
        call  print_colored (COLOR_NORMAL, trim(outstring))
      else
        call  print_colored (COLOR_NOTE, trim(outstring))
      end if
    end if 

    write (outstring,'(SP, 3x, F9.5,A1)') (f_seed - f_best) / max(f_seed, EPSILON) * 100.d0, '%'

    if (improved) then 
      call print_colored (COLOR_GOOD, trim(outstring))
      call print_colored (COLOR_NOTE,' -> Writing design ')
      call print_colored (COLOR_NORMAL,'#'//stri(designcounter))
    else 
      call  print_colored (COLOR_NOTE, trim(outstring))
    end if
    print * 

  end subroutine  show_iteration_result



  subroutine  show_particles_info (overall_best, personal_best, f_values)
    
    !! Shows user info about sucess of a single particle

    use eval, only : OBJ_XFOIL_FAIL, OBJ_DESIGN_FAIL

    double precision, intent(in)  :: overall_best
    double precision, intent(in)  :: personal_best (:), f_values(:)
    integer       :: color, i, ibest 
    character (1) :: sign 

    call print_colored (COLOR_NOTE, ' ')

    ibest = minloc(f_values,1)

    do i = 1, size(f_values)
      if (f_values(i) < overall_best .and. i == ibest) then 
        color = COLOR_GOOD                                ! better then current best
        sign  = '+'
      else if (f_values(i) == OBJ_DESIGN_FAIL) then
        color = COLOR_NOTE                                ! design failed
        sign = '.'
      else if (f_values(i) == OBJ_XFOIL_FAIL) then
        color = COLOR_ERROR                               ! no xfoil convergence
        sign = 'x'
      else if (f_values(i) < personal_best(i)) then
        color = COLOR_NOTE                                ! best of particle up to now
        sign  = '+'
      else  
        color = COLOR_NOTE                                ! no improvement
        sign  = '-'
      end if 

      call print_colored (color, sign)            

    end do 
    
  end subroutine show_particles_info



  subroutine  show_particles_progress (nparticles, ndone)

    !! print a single '.' as progress indicator 
    
    integer, intent(in)  :: nparticles, ndone

    if ((nparticles < 10) .or. (mod(ndone,(nparticles/10)) == 0)) then 
      call print_colored (COLOR_NOTE, '.') 
    end if 
    
  end subroutine show_particles_progress


end module particle_swarm