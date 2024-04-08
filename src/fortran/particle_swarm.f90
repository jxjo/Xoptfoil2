! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2024 Jochen Guenzel


module particle_swarm

  use os_util 
  use print_util

  ! Particle Swarm Optimization - PSO  


  implicit none

  type pso_options_type                 ! Options type definition for PSO

    integer :: pop                      ! particle swarm population size
    double precision :: min_radius      ! min radius of designs before
                                        !   triggering a stop condition
    double precision :: max_speed       ! Max speed allowed for particles
    integer :: max_iterations           ! Max iterations allowed before stopping
    integer :: init_attempts            ! Number of attempts to try to get a feasible
                                        !   initial design
    character(:),allocatable :: convergence_profile
                                        ! 'exhaustive' or 'quick' or 'quick_camb_thick'
                                        ! exhaustive takes onger but finds better 
                                        ! solutions 

    integer :: max_retries = 3          ! max. number of retries a single 
                                        ! particle tries to get a valid geometry
    integer :: auto_frequency = 100     ! how often should auto_retry be tested 
    logical :: rescue_particle = .true. ! rescue stucked particles 
    integer :: stucked_threshold = 15   ! number of iteration until rescue
    integer :: rescue_frequency = 20    ! how often particle resuce action is done  

    logical :: dump_dv = .false.        ! dump design variables to file 

  end type pso_options_type

  contains 


  subroutine particleswarm (dv_0, dv_initial_perturb, pso_options, & 
                            objfunc, &
                            dv_opt, fmin, iteration, fevals, designcounter)

    !----------------------------------------------------------------------------
    !! Particle swarm optimization routine. 
    !! dv_0:    design variables of design 0 which should be seed airfoil 
    !----------------------------------------------------------------------------
                          
    use commons,              only : design_subdir, show_details
    use math_util,            only : norm_2
    use optimization_util,    only : init_random_seed, initial_designs,             &
                                     design_radius, dump_design 
    use optimization_util,    only : reset_run_control, stop_requested, update_run_control
    use optimization_util,    only : write_history_header, write_history

    use eval,                 only : OBJ_XFOIL_FAIL, OBJ_GEO_FAIL, write_progress
    use shape_airfoil,        only : print_dv_as_shape_data

    ! #test 
    ! use eval_constraints,     only : violation_stats_print, violation_stats_reset
    ! use eval,                 only : write_dv_as_shape_data  


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
    integer, intent(out)          :: designcounter


    double precision, dimension(pso_options%pop)  :: objval, minvals, speed
    double precision, dimension(size(dv_0,1))     :: randvec1, randvec2
    double precision, dimension(size(dv_0,1),pso_options%pop) :: dv, vel, bestdesigns
    integer, dimension(pso_options%pop)           :: particle_stuck_counter
    double precision              :: c1, c2, whigh, wlow, convrate, max_speed, wcurr, mincurr, &
                                    radius, brake_factor
    logical                       :: converged, improved, stopped
    character(:), allocatable     :: histfile
    integer                       :: i, fminloc, ndv, j
    integer                       :: i_retry, max_retries, ndone, max_attempts   
    

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
      
    max_attempts = pso_options%init_attempts
    wcurr     = whigh                           ! initial Inertial parameter
    max_speed = pso_options%max_speed           ! speed limit = initial_perturb  
    ndv       = size(dv,1)

    call init_random_seed()                     ! init Fortran random seeds 


    ! --- Set up initial designs

    call initial_designs (dv_0, dv_initial_perturb, max_attempts, dv, objval)

    ! do i = 1, pso_options%pop
    !   call print_dv_as_shape_data (i, dv(:,i))     ! .. overall of all dv velocities of a particle 
    ! end do

    ! Matrix of initial designs for each particle and vector of their values

    bestdesigns = dv                            ! Matrix of best designs for each particle
    minvals = 1d0                               ! vector of their values   (objval)

    ! Global and local best so far

    fmin = 1.0d0                                 ! should be 1.0 
    mincurr = minval(objval,1)
    fminloc = minloc(objval,1)
    dv_opt = dv(:,fminloc)


    ! --- Initial velocities which may be positive or negative

    call random_number(vel)                           ! velocity of all particles for all dv

    vel = max_speed*(vel - 0.5d0)  

    do i = 1, pso_options%pop
        speed(i) = norm_2(vel(:,i))                   ! .. overall of all dv velocities of a particle 
    end do

    ! Counters
    
    fevals = 0                                        ! number of objective evaluations 
    iteration = 0
    designcounter = 0
    particle_stuck_counter = 0                        ! identify particles stucked in solution space

    converged = .false.
    stopped   = .false. 
    max_retries = pso_options%max_retries 

    ! Open file for writing iteration history

    histfile  = design_subdir//'Optimization_History.csv'
    call write_history_header (histfile) 
    call write_history        (histfile, iteration, .false., designcounter, design_radius(dv), fmin)

    ! Write seed airfoil coordinates and polars to file
    call write_progress (dv_0, 0) 
    
    ! init run control with design #0 info 
    call update_run_control (0, 0, fmin)


    ! --- Begin optimization

    call show_optimization_header  (pso_options, show_details)

    !$omp parallel default(shared) private(i, i_retry, j) 
    !$omp barrier

    do while (.not. converged)

      !$omp single
      ! Increase iteration counter

      ndone = 0
      iteration = iteration + 1

      if (pso_options%max_retries > 0) &                  ! retry is dynamic depending on iteration 
        max_retries = auto_max_retries (pso_options, iteration, max_retries, objval) 

      call show_iteration_number (pso_options%max_iterations, iteration, max_retries)
      !$omp end single 

      ! Use OMP DYNAMIC (and not STATIC which is default) so every thread will take a new "i" 
      ! when it finished its task. In summary this is much faster as calculation time differs
      ! very much depending if a xfoil calculation will be made or not (geo constraints!)     

      !$omp do SCHEDULE(DYNAMIC)

      ! Update each particle's position, evaluate objective function, etc.

      do i = 1, pso_options%pop

        ! Impose speed limit - for each dv of each particle!
        do j = 1, ndv
          if (abs(vel(j,i)) > max_speed) then 
            vel(j,i) = max_speed * vel(j,i)/abs(vel(j,i))
          end if  
        end do 

        i_retry = 0

        ! Evaluate objecte function - retry if geometry is violated

        do                                                  ! ... max_retries  

          ! Update position and bring back to side constraints if necessary

          dv(:,i) = dv(:,i) + vel(:,i)

          ! dv crossed boundaries? if yes, step back a little ...

          call check_handle_boundary_violations (dv(:,i), vel(:,i))

          ! Evaluate objective function

          objval(i) = objfunc(dv(:,i))

          ! Valid result --> proceed
          if (objval(i) < OBJ_GEO_FAIL) then 
            exit
          ! Retry if possible 
          elseif (i_retry >= max_retries) then 
            exit
          end if 

          i_retry = i_retry + 1

          ! Invalid result - particle violated geometry - try again with new, smaller velocity
    
          dv(:,i) = dv(:,i) - vel(:,i)                    ! go back to prev position 
          call random_number(brake_factor)
          vel(:,i) = brake_factor * vel(:,i) 
    
        end do 

        ! is particle stucked in solution space? 

        if (objval(i) >= OBJ_XFOIL_FAIL) then                          ! rescue both XFOIL and GEOMETRY
          particle_stuck_counter(i) = particle_stuck_counter(i) + 1    ! increase 'stucked' counter
          if (pso_options%dump_dv) then
            ! call write_dv_as_shape_data (iteration, i, dv(:,i))      ! dump data 
          end if 
        else
          particle_stuck_counter(i) = 0                                ! reset 'get stucked' detect 
        end if 

        !$OMP atomic  
        ndone = ndone + 1

        if (show_details) call show_particles_progress (pso_options%pop, ndone)
        
      end do   

      !$omp end do

      ! wait for all aprticles to finish this iteration 
      !$omp barrier

      ! result evaluation  and particles update --> single threaded
      !$omp master

      ! Display some info about success of single particle 

      call show_particles_info (fmin, minvals, objval, &
                                particle_stuck_counter, pso_options%stucked_threshold)        

      ! Update best overall design, if appropriate

      mincurr = minval(objval,1)
      fminloc = minloc(objval,1)
      if (mincurr < fmin) then
        dv_opt = dv(:,fminloc)
        fmin = mincurr
        improved = .true.
        designcounter = designcounter + 1
      else
        improved = .false.
      end if
      radius = design_radius(dv)


      ! Display result of iteration

      call show_iteration_result (pso_options%min_radius, radius, fmin, designcounter, improved)

      ! Now and then rescue stucked particles before updating particle 

      if (pso_options%rescue_particle .and. (maxval(particle_stuck_counter) >= pso_options%stucked_threshold) &
                                      .and. (mod(iteration, pso_options%rescue_frequency) == 0)) then 
          call rescue_stucked_particles (particle_stuck_counter, pso_options%stucked_threshold, &
                                         dv, fminloc)
      end if 


      ! Update velocity of each particle

      do i = 1, pso_options%pop

        ! Update  best design if appropriate

        if (objval(i) < minvals(i)) then
          minvals(i) = objval(i)
          bestdesigns(:,i) = dv(:,i)
        end if

        call random_number(randvec1)
        call random_number(randvec2)

        ! The incredible Particle Swarm formula ...

        vel(:,i) = wcurr*vel(:,i) + c1*randvec1*(bestdesigns(:,i) - dv(:,i)) +   &
                                    c2*randvec2*(dv_opt - dv(:,i))
        speed(i) = norm_2(vel(:,i))

      end do

      ! Reduce inertial parameter

      wcurr = wcurr - convrate*(wcurr - wlow)
      ! print '(2(A,F7.4),I3)',"   --> wcurr:", wcurr, "  max speed:", maxval(speed,1), maxloc(speed,1)


      ! Evaluate convergence

      if ( (radius > pso_options%min_radius) .and. (iteration < pso_options%max_iterations) ) then
        converged = .false.
      else
        converged = .true.
      end if 

      ! Check for commands in run_control file - reset (touch) run_control

      if (stop_requested()) then
        converged = .true.
        stopped = .true.
      end if
      !$omp end master

      !$omp barrier                             


      ! Write design to file, do dynamic weighting, ... - 

      !$omp single  
      if (improved) then
        call write_progress (dv_opt, designcounter)
      end if
      !$omp end single nowait

      !$omp single  
      call update_run_control(iteration, designcounter, fmin)
      !$omp end single nowait

      ! write history file  
      !$omp single  
      call write_history (histfile, iteration, improved, designcounter, radius, fmin)
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





  subroutine pso_open_particlefile(write_particlefile, particleunit)
    !! testing helper - write all dvs of all particles
    
    logical, intent(in) ::write_particlefile
    integer, intent(inout) :: particleunit
    character(100) :: particlefile
    integer :: ioerr

    if (write_particlefile) then
      ! Set particle file name and identifiers
      particleunit = 20
      particlefile = 'particles.csv'
      print *, "particleswarm: writing particle-values to file "//trim(particlefile)//" ..."
      open(unit=particleunit, file=particlefile, status='replace', iostat=ioerr)
      if (ioerr /= 0) then
        print *, "Error, file-open particles.csv failed !"
        particleunit = 0
        return
      end if
    else
      particleunit = 0
    end if

  end subroutine pso_open_particlefile



  subroutine pso_write_particlefile(particleunit, dv, vel)
    !! testing helper - write all dvs of all particles
    double precision, dimension(:,:), intent(in) :: dv, vel
    integer, intent(inout) :: particleunit
    integer:: nvars, pop, count1, count2

    if (particleunit == 20) then
      nvars = size(dv,1)
      pop = size(dv,2)
      do count1 = 1, pop
        do count2 = 1, nvars
          ! Write all particle-values without linefeed 
          write(particleunit,'(2F12.6)', advance='NO') dv(count2,count1)
          write(particleunit, '(A)', advance='NO') ";"
          write(particleunit,'(2F12.6)', advance='NO') vel(count2,count1)
          !Separator between particle-values
          write(particleunit, '(A)', advance='NO') ";"
        end do
        ! Separator between particles
        write(particleunit, '(A)', advance='NO') ";"
      end do
      ! Write linefeed
      write(particleunit, *) ""
    end if

  end subroutine pso_write_particlefile



  subroutine pso_close_particlefile(particleunit)
    !! testing helper - write all dvs of all particles
    integer, intent(inout) :: particleunit
      
    if (particleunit == 20) then
      flush(particleunit)
      close(particleunit)
      particleunit = 0
    end if    
  end subroutine pso_close_particlefile


  function auto_max_retries (pso_options, iteration, cur_max_retries, objval) 

    !! Adopt max_retries of particles according to iterations and 
    !!   percentage of geometry failed particles 

    use eval, only : OBJ_GEO_FAIL

    type (pso_options_type), intent(in) :: pso_options
    integer             :: auto_max_retries
    integer, intent(in) :: iteration, cur_max_retries
    double precision, dimension(:), intent(in) :: objval

    integer          :: ngeo_fails 
    integer          :: nparticles, i
    double precision :: fail_percentage

    auto_max_retries = cur_max_retries

    if (mod(iteration, pso_options%auto_frequency) /= 0) then      ! new value only when frequency
      return        
    end if 

    ! Count no of geometry fails of particles 
    ngeo_fails = 0
    nparticles = size(objval) 
    do i = 1, nparticles
      if (objval(i) >= OBJ_GEO_FAIL) ngeo_fails = ngeo_fails + 1    ! no valid design  
    end do 

    fail_percentage = 100d0 * ngeo_fails / nparticles 

    !write (*,'(/,A,F4.0,A,/)') '---- New Auto_retry at ', fail_percentage,'%'
    if (fail_percentage > 75d0) then 
      auto_max_retries = pso_options%max_retries
    elseif (fail_percentage < 50d0) then 
      auto_max_retries = 0
    end if
    
  end function auto_max_retries



  subroutine  show_optimization_header  (pso_options, show_details)

    !! Shows user info - header of optimization out 

    type (pso_options_type), intent(in) :: pso_options
    logical, intent(in)                 :: show_details

    integer            :: retr,freq          
    character(200)     :: blanks = ' '
    character(:), allocatable     :: var_string, progress

    retr = pso_options%max_retries
    freq = pso_options%auto_frequency

    if (retr > 0 ) then   
      call print_note ("Auto retry of particle having failed geometry. "//&
                       "Starting with r="//stri(retr), 3)
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


  subroutine check_handle_boundary_violations (dv, vel)
    
    !! checks if a particle having dv violates boundaries 0..1 
    !!    repairs violations by changing dv and vel 

    double precision, intent(inout) :: dv(:), vel(:)    

    integer           :: idv, ndv 
    double precision  :: prev_dv
  
    ndv = size (dv) 

    do idv = 1, ndv                              
      if (dv(idv) < 0d0) then

        prev_dv = dv(idv) - vel(idv)                  ! back to previous position  
        dv(idv) = prev_dv / 2d0                       ! new pos half way to 0.0 
        vel(idv) = dv(idv) - prev_dv                  ! recalc velocity 

      elseif (dv(idv) > 1d0) then

        prev_dv = dv(idv) - vel(idv)                  ! back to previous position 
        dv(idv) = prev_dv + (1d0 - prev_dv) / 2d0     ! new pos half way to 1.0 
        vel(idv) = dv(idv) - prev_dv                  ! recalc velocity 

      end if
    end do

  end subroutine



  subroutine  rescue_stucked_particles (particle_stuck_counter, threshold, dv, fminloc)
    
    !! single particles cannot achieve to get out of a xfoil failure situation 
    !!    Set these stucked particles to swarms best (still having different velocity)

    integer, dimension (:), intent(inout)            :: particle_stuck_counter
    double precision, dimension (:,:), intent(inout) :: dv 
    integer, intent(in)          :: threshold, fminloc

    integer       :: i, color  
    character (1) :: sign 

    print *
    call print_colored (COLOR_FEATURE,' - Rescuing stucked particles ')

    do i = 1, size(particle_stuck_counter)

      if (particle_stuck_counter(i) >= threshold) then 

        ! stucked will get new prime position von swarm best 
        dv(:,i) = dv(:, fminloc)

        ! reset stuck counter
        particle_stuck_counter(i) = 0 
        color = COLOR_FEATURE                               ! mark as rescued
        sign  = '#'
      else  
        color = COLOR_NOTE                                  ! nothing done 
        sign  = '-'
      end if 

      call print_colored (color, sign)      
    end do 
    
    print * 
    print * 
    
  end subroutine rescue_stucked_particles 



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



  subroutine  show_particles_info (overall_best, personal_best, objval, &
                                   stuck_counter, stucked_threshold)
    
    !! Shows user info about sucess of a single particle

    use eval, only : OBJ_XFOIL_FAIL, OBJ_GEO_FAIL

    double precision, intent(in)  :: overall_best
    double precision, intent(in)  :: personal_best (:), objval(:)
    integer, intent(in)           :: stuck_counter (:), stucked_threshold
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
        if (stuck_counter(i) > stucked_threshold) then    ! ... and stucked for a while 
          sign = '#'
        else 
          sign = 'x'
        end if 
      else if (objval(i) < personal_best(i)) then 
        color = COLOR_NOTE                                ! best of particle up to now
        sign  = '+'
      else if (objval(i) >= OBJ_GEO_FAIL) then 
        color = COLOR_NOTE                                ! no valid design 
        if (stuck_counter(i) > stucked_threshold) then    ! ... and stucked for a while 
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


end module particle_swarm