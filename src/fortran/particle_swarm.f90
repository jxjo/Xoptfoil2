! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2025 Jochen Guenzel


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
                                        ! 'exhaustive' or 'quick' or 'quick_camb_thick'
                                        ! exhaustive takes onger but finds better 
                                        ! solutions 

    integer :: max_retries = 3          ! max. number of retries a single 
                                        ! particle tries to get a valid geometry
    logical :: rescue_particle = .true. ! rescue stucked particles 
    integer :: stucked_threshold = 15   ! number of iteration until rescue
    integer :: rescue_frequency = 20    ! how often particle resuce action is done  

    logical :: dump_dv = .false.        ! dump design variables to file 

  end type pso_options_type

  contains 


  subroutine particleswarm (dv_0, dv_initial_perturb, pso_options, & 
                            objfunc, &
                            dv_opt, fmin, iteration, fevals)

    !----------------------------------------------------------------------------
    !! Particle swarm optimization routine. 
    !! dv_0:    design variables of design 0 which should be seed airfoil 
    !----------------------------------------------------------------------------
                          
    use commons,              only : design_subdir, show_details
    use math_util,            only : norm_2
    use optimization_util,    only : init_random_seed, initial_designs,             &
                                     design_radius, dump_design, clip_velocity, initial_velocities 
    use optimization_util,    only : reset_run_control, stop_requested, update_run_control
    use optimization_util,    only : write_history_header, write_history

    use eval,                 only : OBJ_XFOIL_FAIL, OBJ_GEO_FAIL, write_progress, geo_penalty, is_design_valid
    use shape_airfoil,        only : print_dv_as_shape_data



    double precision, intent(in)        :: dv_0 (:), dv_initial_perturb (:) 
    type (pso_options_type), intent(in) :: pso_options
    double precision, intent(inout)     :: dv_opt (:)

    interface
      double precision function objfunc(v)
        double precision, intent(in) :: v (:)
      end function
    end interface
    
    double precision, intent(out) :: fmin
    integer, intent(out)          :: iteration, fevals

    double precision, dimension(pso_options%pop)  :: obj_val, best_val
    double precision, dimension(size(dv_0,1))     :: c1_random, c2_random, new_vel
    double precision, dimension(size(dv_0,1),pso_options%pop) :: dv, vel, best_design
    integer, dimension(pso_options%pop)           :: stuck_counter
    double precision              :: c1, c2, whigh, wlow, convrate, max_speed, wcurr, min_obj_val, &
                                    radius, speed_scale
    logical                       :: converged, improved, stopped
    character(:), allocatable     :: histfile
    integer                       :: i, i_min, ndv, i_retry, ndesigns
    integer                       :: max_retries, ndone  
    

    call print_header ('Particle swarm with '//stri(pso_options%pop)// ' members will now try its best ...')


    ! PSO tuning variables

    if (pso_options%convergence_profile == "quick") then

      c1 = 1.2d0                      ! particle-best trust factor
      c2 = 1.2d0                      ! swarm-best trust factor
      whigh = 1.4d0                   ! starting inertial parameter
      wlow = 0.6d0                    ! ending inertial parameter
      convrate = 0.05d0               ! inertial parameter reduction rate

    else if (pso_options%convergence_profile == "exhaustive") then

      c1 = 1.4d0                       ! particle-best trust factor
      c2 = 1.0d0                       ! swarm-best trust factor
      whigh = 1.8d0                    ! starting inertial parameter
      wlow = 0.65d0 ! 0.8d0            ! ending inertial parameter - better lower and smaller convrate
      convrate = 0.012d0 ! 0.02d0      ! inertial parameter reduction rate

    else if (pso_options%convergence_profile == "quick_camb_thick") then

      c1 = 1.0d0                      ! particle-best trust factor
      c2 = 1.6d0                      ! swarm-best trust factor
      whigh = 1.2d0                   ! starting inertial parameter
      wlow = 0.1d0                    ! ending inertial parameter
      convrate = 0.07d0     ! 0.025d0 ! inertial parameter reduction rate

    else
      call my_stop ("Unknown convergence_profile: "// pso_options%convergence_profile)
    end if
      
    wcurr     = whigh                           ! initial Inertial parameter
    max_speed = pso_options%max_speed           ! speed limit = initial_perturb  
    ndv       = size(dv,1)

    call init_random_seed()                     ! init Fortran random seeds 


    ! --- Set up initial designs and their obj values

    call initial_designs (dv_0, dv_initial_perturb, dv, obj_val)

    ! Initial velocities 
    
    vel = initial_velocities (dv, max_speed)

    ! Global and local best so far

    best_design = dv                                  ! Matrix of best designs for each particle
    best_val    = obj_val                             ! vector of their values   (objval)

    fmin        = 1.0d0                               ! particle #1 is dv0 -> 1.0
    dv_opt      = dv(:,1)                             ! best design so far is dv0 


    ! Counters
    
    fevals        = 0                                 ! initial evaluation count
    iteration     = 0
    ndesigns = 0
    converged     = .false.
    stopped       = .false. 
    max_retries   = pso_options%max_retries 

    stuck_counter = 0                                   ! identify particles stucked in solution space

    ! Open file for writing iteration history

    histfile  = design_subdir//'Optimization_History.csv'
    call write_history_header (histfile) 
    call write_history        (histfile, iteration, .false., ndesigns, design_radius(dv), fmin)

    ! Write seed airfoil coordinates and polars to file
    call write_progress (dv_0, 0) 
    
    ! init run control with design #0 info 
    call update_run_control (0, 0, fmin)


    ! --- Begin optimization


    call show_optimization_header  (pso_options, show_details)

    !$omp parallel default(shared) private(i, i_retry) 

    do while (.not. converged)

      !$omp master
      ndone = 0
      iteration = iteration + 1

      call show_iteration_number (pso_options%max_iterations, iteration, max_retries)
      !$omp end master
      !$omp barrier

      ! Use OMP DYNAMIC (and not STATIC which is default) so every thread will take a new "i" 
      ! when it finished its task. In summary this is much faster as calculation time differs
      ! very much depending if a xfoil calculation will be made or not (geo constraints!)     

      !$omp do SCHEDULE(DYNAMIC)

      ! Update each particle's position, evaluate objective function, etc.

      do i = 1, pso_options%pop

        dv(:,i)    = dv(:,i) + vel(:,i)               ! update position

        obj_val(i) = objfunc(dv(:,i))                 ! evaluate objective at new position

        !$omp atomic
        ndone = ndone + 1
        if (show_details) call show_particles_progress (pso_options%pop, ndone)
       
      end do   

      !$omp end do

      !$omp barrier
      !$omp master

      ! result evaluation  and particles update --> single threaded

      call show_particles_info (pso_options, fmin, best_val, obj_val, stuck_counter)

      ! Update best overall 

      min_obj_val = minval(obj_val,1)
      if (min_obj_val < fmin) then
        fmin      = min_obj_val
        i_min     = minloc(obj_val,1)
        dv_opt    = dv(:,i_min)
        improved  = fmin < 1d0
        ndesigns  = ndesigns + 1
      else
        improved  = .false.
      end if

      radius = design_radius(dv)

      ! Display result of iteration

      call show_iteration_result (pso_options%min_radius, radius, fmin, ndesigns, improved)

      ! Update stuck counters and rescue particles if needed

      call handle_stuck_particles (pso_options, iteration, obj_val, stuck_counter, dv, dv_opt)

      ! Update velocity of each particle

      do i = 1, pso_options%pop

        ! Update  best design if appropriate

        if (obj_val(i) < best_val(i)) then
          best_val(i)      = obj_val(i)
          best_design(:,i) = dv(:,i)
        end if

        speed_scale = 1.0d0
        i_retry = 0

        do 

          ! The incredible Particle Swarm formula with new random factors each retry

          call random_number(c1_random)
          call random_number(c2_random)

          new_vel = wcurr * vel(:,i) + &                                    ! inertia
                    c1 * c1_random * (best_design(:,i) - dv(:,i)) +   &     ! particle best
                    c2 * c2_random * (dv_opt - dv(:,i))                     ! swarm best

          new_vel = speed_scale * new_vel                                   ! gentle reduction for retries
          new_vel = clip_velocity (dv(:,i), max_speed, new_vel)             ! ensure maxspeed and boundary respect

          if (is_design_valid (dv(:,i) + new_vel) .or. i_retry >= pso_options%max_retries ) then 
            exit
          end if

          i_retry = i_retry + 1
          speed_scale = speed_scale * 0.9d0   ! if not valid, reduce speed more for next retry

        end do 

        vel(:,i) = new_vel

        ! call debug_print_retry (i, i_retry, speed_scale, new_vel)

      end do

      ! call debug_print_obj_val (obj_val, best_val, fmin, improved)

      ! Reduce inertial parameter

      wcurr = wcurr - convrate*(wcurr - wlow)

      ! Evaluate convergence

      if ( (radius > pso_options%min_radius) .and. (iteration < pso_options%max_iterations) ) then
        converged = .false.
      else
        converged = .true.
      end if 

      ! Check for commands in run_control file - reset (touch) run_control

      if (stop_requested()) then
        converged = .true.
        stopped   = .true.
      end if

      !$omp end master
      !$omp barrier                             

      ! Write design to file, update run control, write history - all I/O in one block 

      !$omp single  
      if (improved) then
        call write_progress (dv_opt, ndesigns)
      end if
      call update_run_control (iteration, ndesigns, fmin)
      call write_history (histfile, iteration, improved, ndesigns, radius, fmin)
      !$omp end single nowait

      ! let all particles start together a new round 
      !$omp barrier                             

    end do 

    !$omp end parallel

    if (stopped) then
      print *
      call print_colored (COLOR_WARNING, '   Stop command')
      call print_text ('encountered in run_control')
    else if (iteration >= pso_options%max_iterations) then
      print * 
      call print_colored (COLOR_WARNING, '   Max number')
      call print_text ('of iterations reached')
    end if

        
    ! Calculate number of function evaluations
    fevals = fevals + iteration*pso_options%pop         ! this is not correct as we have retries 

  end subroutine particleswarm



  subroutine  show_optimization_header  (pso_options, show_details)

    !! Shows user info - header of optimization out 

    type (pso_options_type), intent(in) :: pso_options
    logical, intent(in)                 :: show_details

    integer            :: retr       
    character(200)     :: blanks = ' '
    character(:), allocatable     :: var_string, progress

    retr = pso_options%max_retries
    if (retr > 0 ) then   
      call print_note ("Upto "//stri(retr)//" retry of particle having failed geometry.", 3)
    end if

    if (pso_options%rescue_particle) then   
      call print_note ("Rescuing particles being stucked for "//&
                       stri(pso_options%stucked_threshold)//" iterations", 3) 
    end if

    print *
    write(*,'(3x)', advance = 'no')
    call  print_colored (COLOR_NOTE,  "Particle result:  '")
    call  print_colored (COLOR_GOOD,  "+")
    call  print_colored (COLOR_NOTE,  "' new swarm best  '+' personal best   '-' not better")
    write(*,'(/,3x)', advance = 'no')
    call  print_colored (COLOR_NOTE,  "                  '")
    call  print_colored (COLOR_ERROR, "x")
    call  print_colored (COLOR_NOTE,  "' xfoil no conv   '")
    call  print_colored (COLOR_NOTE, "#")
    call  print_colored (COLOR_NOTE,  "' stucked no conv ' ' geometry failed")
    print *

    print *

    ! particle progress dependent of show details 
    if (show_details) then 
      progress = 'Progress   '
    else 
      progress = ' ' 
    end if 

    ! complicated line output ...
    var_string = 'Particles result' // blanks (len('Particles result') : pso_options%pop)
    write(*,'(3x,A6,3x, A, A,          1x,A6,   5x)', advance ='no') &
            'Iterat',progress, var_string,'Radius'
    
    write(*,'(A11)') 'Improvement'

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



  subroutine  show_iteration_result (min_radius, radius, fmin, designcounter, improved)

    !! Shows user info about result of a single iteration 

    double precision, intent(in)  :: min_radius, radius ,fmin 
    logical, intent(in)           :: improved
    integer, intent(in)           :: designcounter
    character(25)                 :: outstring

    write (outstring,'(ES9.1)') radius
    if (radius < min_radius) then
      call  print_colored (COLOR_FEATURE, trim(outstring))
    else
      if (improved) then 
        call  print_colored (COLOR_NORMAL, trim(outstring))
      else
        call  print_colored (COLOR_NOTE, trim(outstring))
      end if
    end if 

    write (outstring,'(SP, 3x, F9.5,A1)') (1.0d0 - fmin) * 100.d0, '%'

    if (improved) then 
      call print_colored (COLOR_GOOD, trim(outstring))
      call print_colored (COLOR_NOTE,' -> Writing design ')
      call print_colored (COLOR_NORMAL,'#'//stri(designcounter))
    else 
      call  print_colored (COLOR_NOTE, trim(outstring))
    end if
    print * 

  end subroutine  show_iteration_result



  subroutine handle_stuck_particles (pso_options, iteration, obj_val, stuck_counter, dv, dv_opt)
    
    !! Update stuck counters for all particles and rescue them if needed

    use eval, only : OBJ_GEO_FAIL

    type (pso_options_type), intent(in) :: pso_options
    integer, intent(in)                 :: iteration
    double precision, intent(in)        :: obj_val(:), dv_opt(:)
    integer, intent(inout)              :: stuck_counter(:)
    double precision, intent(inout)     :: dv(:,:)

    integer :: i, color
    character (1) :: sign
    logical :: need_rescue

    if (.not. pso_options%rescue_particle) return

    ! Update stuck counter for each particle

    do i = 1, size(obj_val)
      if (obj_val(i) < OBJ_GEO_FAIL) then 
        stuck_counter(i) = 0
      else 
        stuck_counter(i) = stuck_counter(i) + 1
      end if
    end do

    ! Rescue particles if threshold exceeded and at rescue frequency

    need_rescue = (maxval(stuck_counter) >= pso_options%stucked_threshold) &
                  .and. (mod(iteration, pso_options%rescue_frequency) == 0)

    if (need_rescue) then
      print *
      call print_colored (COLOR_FEATURE,' - Rescuing stucked particles ')

      do i = 1, size(stuck_counter)
        if (stuck_counter(i) >= pso_options%stucked_threshold) then 
          ! Stucked particle gets new position from swarm best 
          dv(:,i) = dv_opt
          stuck_counter(i) = 0 
          color = COLOR_FEATURE
          sign  = '#'
        else  
          color = COLOR_NOTE
          sign  = '-'
        end if 
        call print_colored (color, sign)      
      end do 
      
      print * 
      print * 
    end if

  end subroutine handle_stuck_particles



  subroutine  show_particles_info (pso_options, overall_best, personal_best, objval, &
                                   stuck_counter)
    
    !! Shows user info about sucess of a single particle

    use eval, only : OBJ_XFOIL_FAIL, OBJ_GEO_FAIL

    type (pso_options_type), intent(in) :: pso_options
    double precision, intent(in)  :: overall_best
    double precision, intent(in)  :: personal_best (:), objval(:)
    integer, intent(in)           :: stuck_counter (:)
    integer       :: color, i, ibest 
    character (1) :: sign 

    call print_colored (COLOR_NOTE, ' ')

    ibest = minloc(objval,1)

    do i = 1, size(objval)
      if (objval(i) < overall_best .and. i == ibest) then 
        color = COLOR_GOOD                                ! better then current best
        sign  = '+'
      else if (objval(i) == OBJ_XFOIL_FAIL) then     
        color = COLOR_ERROR                               ! no xfoil convergence
        if (stuck_counter(i) > pso_options%stucked_threshold) then    ! ... and stucked for a while 
          sign = '#'
        else 
          sign = 'x'
        end if 
      else if (objval(i) < personal_best(i)) then 
        color = COLOR_NOTE                                ! best of particle up to now
        sign  = '+'
      else if (objval(i) >= OBJ_GEO_FAIL) then 
        color = COLOR_NOTE                                ! no valid design 
        if (stuck_counter(i) > pso_options%stucked_threshold) then    ! ... and stucked for a while 
          sign = '#'
        else 
          sign = ' '
        end if 
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



  subroutine debug_print_obj_val (obj_val, best_val, fmin, improved)

    !! Debug output: print objective values, best values, and overall best

    double precision, intent(in) :: obj_val(:), best_val(:), fmin
    logical, intent(in)          :: improved

    write (*,'(A, 30F8.4)') "obj  val ", obj_val
    write (*,'(A, 30F8.4)') "best val ", best_val
    write (*,'(A, I2, 2F10.7, L)') "best ", minloc(best_val,1), fmin, minval (obj_val,1), improved

  end subroutine debug_print_obj_val



  subroutine debug_print_retry (particle_num, retry_count, speed_scale, velocity)

    !! Debug output: print retry information for a particle

    use math_util, only : norm_2

    integer, intent(in)          :: particle_num, retry_count
    double precision, intent(in) :: speed_scale, velocity(:)

    if (retry_count > 0) then 
      print *, "P ", stri(particle_num,2), " retry ", stri(retry_count,2), & 
               "  scale: ", strf('(F6.3)',speed_scale), & 
               "   vel: ", strf('(F6.3)',norm_2(velocity))
    end if

  end subroutine debug_print_retry


end module particle_swarm