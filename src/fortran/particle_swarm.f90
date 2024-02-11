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
    integer :: max_iterations           ! Max steps allowed before stopping
    integer :: init_attempts            ! Number of attempts to try to get a feasible
                                        !   initial design
    character(:),allocatable :: convergence_profile
                                        ! 'exhaustive' or 'quick' or 'quick_camb_thick
                                        ! exhaustive takes onger but finds better 
                                        ! solutions 

    integer :: max_retries = 3          ! max. number of retries a single 
                                        ! particle tries to get a valid geometry
    integer :: auto_frequency = 100     ! #how often should auto_retry be tested 

    logical :: rescue_particle = .true. ! resque stucked particles 
    integer :: stucked_threshold = 10   ! number of iteration steps until rescue
    integer :: rescue_frequency = 10    ! how often particle resuce action is done  


    logical :: dump_dv = .false.        ! dump design variables to file 

    double precision :: c1 = 0d0        ! particle-best trust factor
    double precision :: c2 = 0d0        ! swarm-best trust factor
    double precision :: whigh = 0d0     ! starting inertial parameter
    double precision :: wlow = 0d0      ! ending inertial parameter
    double precision :: convrate = 0d0  ! inertial parameter reduction rate

  end type pso_options_type

  contains 


  subroutine particleswarm (dv_0, dv_initial_perturb, pso_options, & 
                            objfunc, &
                            dv_opt, fmin, step, fevals, designcounter)

    !----------------------------------------------------------------------------
    !! Particle swarm optimization routine. 
    !! dv_0:    design variables of design 0 which should be seed airfoil 
    !----------------------------------------------------------------------------
                          
    use commons,              only : design_subdir, show_details
    use math_deps,            only : norm_2
    use optimization_util,    only : init_random_seed, initial_designs,             &
                                     design_radius, dump_design 
    use optimization_util,    only : reset_run_control, stop_requested
    use optimization_util,    only : write_history_header, write_history

    use eval,                 only : OBJ_XFOIL_FAIL, OBJ_GEO_FAIL, write_progress

    ! #test 
    ! use eval_constraints,     only : violation_stats_print, violation_stats_reset
    ! use eval,                 only : write_dv_as_shape_data  ! #todo move to shape 


    double precision, intent(in)        :: dv_0 (:), dv_initial_perturb (:) 
    type (pso_options_type), intent(in) :: pso_options
    double precision, intent(inout)     :: dv_opt (:)

    interface
      double precision function objfunc(v)
        double precision, intent(in) :: v (:)
      end function
    end interface
    
    double precision, intent(out) :: fmin
    integer, intent(out)          :: step, fevals
    integer, intent(out)          :: designcounter


    double precision, dimension(pso_options%pop) :: objval, minvals, speed, particles_stucked
    double precision, dimension(size(dv_0,1)) :: randvec1, randvec2
    double precision, dimension(size(dv_0,1),pso_options%pop) :: dv, vel, bestdesigns
    double precision              :: c1, c2, whigh, wlow, convrate, max_speed, wcurr, mincurr, &
                                    radius, brake_factor, prev_dv
    logical                       :: converged, signal_progress
    character(:), allocatable     :: histfile
    integer                       :: i, idv, fminloc, ndv
    integer                       :: i_retry, max_retries, ndone, max_attempts     
    

    ! PSO tuning variables

    if (pso_options%convergence_profile == "quick") then

      c1 = 1.2d0         ! particle-best trust factor
      c2 = 1.2d0         ! swarm-best trust factor
      whigh = 1.4d0      ! starting inertial parameter
      wlow = 0.6d0       ! ending inertial parameter
      convrate = 0.05d0  ! inertial parameter reduction rate

    else if (pso_options%convergence_profile == "exhaustive") then

      c1 = 1.4d0         ! particle-best trust factor
      c2 = 1.0d0         ! swarm-best trust factor
      whigh = 1.8d0      ! starting inertial parameter
      wlow = 0.8d0       ! ending inertial parameter
      convrate = 0.02d0  ! inertial parameter reduction rate

    else if (pso_options%convergence_profile == "quick_camb_thick") then

      c1 = 1.0d0         ! particle-best trust factor
      c2 = 1.6d0         ! swarm-best trust factor
      whigh = 1.2d0      ! starting inertial parameter
      wlow = 0.02d0      ! ending inertial parameter
      convrate = 0.025d0 ! inertial parameter reduction rate

    else
      call my_stop ("Unknown convergence_profile: "// pso_options%convergence_profile)
    end if

    ! allow direct manipulation of parms in inputs 

    if (pso_options%c1 > 0d0)       c1        = pso_options%c1
    if (pso_options%c2 > 0d0)       c2        = pso_options%c2
    if (pso_options%whigh > 0d0)    whigh     = pso_options%whigh
    if (pso_options%wlow > 0d0)     wlow      = pso_options%wlow
    if (pso_options%convrate > 0d0) convrate  = pso_options%convrate

      
    max_attempts = pso_options%init_attempts
    wcurr     = whigh                           ! initial Inertial parameter
    max_speed  = pso_options%max_speed           ! speed limit = initial_perturb  
    ndv       = size(dv,1)

    call init_random_seed()                     ! init Fortran random seeds 


    ! --- Set up initial designs

    call initial_designs (dv_0, dv_initial_perturb, max_attempts, dv, objval)

    ! Matrix of initial designs for each particle and vector of their values

    bestdesigns = dv                            ! Matrix of best designs for each particle
    minvals = 1d0                               ! vector of their valuess   (objval)

    ! Global and local best so far

    fmin = 1.0d0                                 ! should be 1.0 
    mincurr = minval(objval,1)
    fminloc = minloc(objval,1)
    dv_opt = dv(:,fminloc)


    ! --- Initial velocities which may be positive or negative

    call random_number(vel)                     ! velocity of all particles for all dv

    vel = max_speed*(vel - 0.5d0)  

    ! #test 
    ! print *, "speed dv_init", norm_2(dv_initial_perturb)
    ! vel = vel - 0.5d0  
    ! do i = 1, pso_options%pop
    !   vel(:,i) = vel(:,i) * dv_initial_perturb             ! .. overall of all dv velocities of a particle 
    ! end do


    do i = 1, pso_options%pop
        speed(i) = norm_2(vel(:,i))             ! .. overall of all dv velocities of a particle 
    end do

    ! Counters
    
    fevals = 0                                  ! number of objective evaluations 
    step = 0
    designcounter = 0
    particles_stucked = 0                        ! identify particles stucked in solution space

    converged = .false.
    max_retries = pso_options%max_retries 

    ! user info 

    print *
    if (show_details) then 
      call  print_colored (COLOR_FEATURE, ' - Particle swarm ')
      call  print_colored (COLOR_NORMAL, 'with '//stri(pso_options%pop)// ' members will now try its best ...')
      print *
      print *
    end if 

    ! Open file for writing iteration history

    histfile  = design_subdir//'Optimization_History.csv'
    call write_history_header (histfile) 
    call write_history        (histfile, step, .false., designcounter, design_radius(dv), fmin)

    ! Write seed airfoil coordinates and polars to file

    call write_progress (dv_0, 0) 


    ! --- Begin optimization

    call show_optimization_header  (pso_options)

    !$omp parallel default(shared) private(i, idv, i_retry, prev_dv)

    do while (.not. converged)

      !$omp master

      ! Increase iteration counter

      step = step + 1

      if (pso_options%max_retries > 0) &
        max_retries = auto_max_retries (pso_options, step, max_retries, objval) 

      if (show_details) call show_iteration_number (step, max_retries)

      !$omp end master
      ndone= 0
      !$omp barrier

      ! Use OMP DYNAMIC (and not STATIC which is default) so every thread will take a new "i" 
      ! when it finished its task. In summary this is much faster as calculation time differs
      ! very much depending if a xfoil calculation will be made or not (geo constraints!)     

      !$omp do SCHEDULE(DYNAMIC)

      ! Update each particle's position, evaluate objective function, etc.

      do i = 1, pso_options%pop

        ! Impose speed limit
        if (speed(i) > max_speed) then
          vel(:,i) = max_speed*vel(:,i)/speed(i)
        end if

        ! # test speed 
        ! do idv = 1, ndv 
        !   if (abs(vel(idv,i)) > abs(dv_initial_perturb(idv))) then 
        !     vel(idv,i) = vel(idv,i) * abs(vel(idv,i)) / abs(dv_initial_perturb(idv))
        !   end if 
        ! end do 

        i_retry = 0

        ! Evaluate objecte function - retry if geometry is violated

        do                            ! ... max_retries  

          ! Update position and bring back to side constraints if necessary

          dv(:,i) = dv(:,i) + vel(:,i)

          ! dv crossed boundaries? if yes, step back a little ...
          do idv = 1, ndv                              
            if (dv(idv,i) < 0d0) then

              prev_dv = dv(idv,i) - vel(idv,i)              ! back to previous position  
              dv(idv,i) = prev_dv / 2d0                     ! new pos half way to 0.0 
              vel(idv,i) = dv(idv,i) - prev_dv              ! recalc velocity 

            elseif (dv(idv,i) > 1d0) then

              prev_dv = dv(idv,i) - vel(idv,i)              ! back to previous position 
              dv(idv,i) = prev_dv + (1d0 - prev_dv) / 2d0   ! new pos half way to 1.0 
              vel(idv,i) = dv(idv,i) - prev_dv              ! recalc velocity 

            end if
          end do

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

        if (objval(i) >= OBJ_XFOIL_FAIL) then                     ! rescue both XFOIL and GEOMETRY
          particles_stucked(i) = particles_stucked(i) + 1         ! increase 'stucked' counter
          if (pso_options%dump_dv) then
            ! call write_dv_as_shape_data (step, i, dv(:,i))      ! dump data 
          end if 
        else
          particles_stucked(i) = 0                                ! reset 'get stucked' detect 
        end if 

        !$OMP ATOMIC  
        ndone = ndone + 1
        if (show_details) call show_particles_progress (pso_options%pop, ndone)

      end do   

      !$omp end do

      !$omp master

      ! Display some info about success of single particle 

      if (.not. show_details) call show_iteration_number (step, max_retries)

      call show_particles_info (fmin, minvals, particles_stucked, objval)        

      ! Update best overall design, if appropriate

      mincurr = minval(objval,1)
      fminloc = minloc(objval,1)
      if (mincurr < fmin) then
        dv_opt = dv(:,fminloc)
        fmin = mincurr
        signal_progress = .true.
      else
        signal_progress = .false.
      end if


      ! Display and write progress 

      radius = design_radius(dv)
      call show_iteration_result (radius, fmin, signal_progress)

      ! Write design to file - 

      if (signal_progress) then
        designcounter = designcounter + 1
        call write_progress (dv_opt, designcounter)
      else
        write (*,*)
      end if

      ! write history 

      call write_history (histfile, step, signal_progress, designcounter, radius, fmin)


      ! Now and then rescue stucked particles before updating particle 
    
      if (pso_options%rescue_particle) then
        if ((maxval(particles_stucked) >= pso_options%stucked_threshold) .and. &
            (mod(step, pso_options%rescue_frequency) == 0)) then 

          call rescue_stucked_particles (particles_stucked, pso_options%stucked_threshold, &
                                         dv, fminloc, bestdesigns)
        end if 
      end if 

      !$omp end master
      !$omp barrier

      !$omp do

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

      !$omp end do

      !$omp master

      ! Reduce inertial parameter

      wcurr = wcurr - convrate*(wcurr - wlow)
  
      ! Evaluate convergence

      if ( (radius > pso_options%min_radius) .and. (step < pso_options%max_iterations) ) then
        converged = .false.
      else
        converged = .true.
        if (step == pso_options%max_iterations) then
          write (*,*)
          call print_warning ('PSO optimizer stopped due to the max number of iterations being reached.')
        end if
      end if 

      ! Check for commands in run_control file - reset (touch) run_control

      if (stop_requested()) then
        converged = .true.
        print *
        call print_action  ('Cleaning up: stop command encountered in run_control', .true.)
      end if

      call reset_run_control()


      !$omp end master
      !$omp barrier

    end do 

    !$omp end parallel

    ! Calculate number of function evaluations
        
    fevals = fevals + step*pso_options%pop

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


  function auto_max_retries (pso_options, step, cur_max_retries, objval) 

    !! Adopt max_retries of particles according to iteration steps and 
    !!   percentage of geometry failed particles 

    use eval, only : OBJ_GEO_FAIL

    type (pso_options_type), intent(in) :: pso_options
    integer             :: auto_max_retries
    integer, intent(in) :: step, cur_max_retries
    double precision, dimension(:), intent(in) :: objval

    integer          :: ngeo_fails 
    integer          :: nparticles, i
    double precision :: fail_percentage

    auto_max_retries = cur_max_retries

    if (mod(step, pso_options%auto_frequency) /= 0) then      ! new value only when frequency
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



  subroutine  show_optimization_header  (pso_options)

    !! Shows user info - header of optimization out 

    type (pso_options_type), intent(in) :: pso_options

    integer            :: retr,freq          
    character(200)     :: blanks = ' '
    character(:), allocatable     :: var_string

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
    call  print_colored (COLOR_NOTE,  "' new swarm best  '+' personal best   '-' not better  '")
    write(*,'(/,3x)', advance = 'no')
    call  print_colored (COLOR_NOTE,  "                  '")
    call  print_colored (COLOR_ERROR, "x")
    call  print_colored (COLOR_NOTE,  "' xfoil no conv   '")
    call  print_colored (COLOR_ERROR, "#")
    call  print_colored (COLOR_NOTE,  "' stucked no conv ' ' geometry failed")
    write (*,*)

    print *

    var_string = 'Particles result' // blanks (len('Particles result') : pso_options%pop)
    write(*,'(3x,A6,3x, A, A,          1x,A6,   5x)', advance ='no') &
            'Iterat','Progress   ', var_string,'Radius'
    
    write(*,'(A11)') 'Improvement'

  end subroutine show_optimization_header



  subroutine  rescue_stucked_particles (particles_stucked, threshold, dv, fminloc, bestdesigns)
    
    !! single particles cannot achieve to get out of a xfoil failure situation 
    !!    Set these stucked particles to swarms best (still having different velocity)

    double precision, dimension (:),   intent(inout) :: particles_stucked 
    double precision, dimension (:,:), intent(inout) :: dv, bestdesigns   
    integer, intent(in)          :: threshold, fminloc

    integer       :: i, color  
    character (1) :: sign 

    write (*,*) 
    call print_colored (COLOR_FEATURE,' - Rescuing stucked particles ')

    do i = 1, size(particles_stucked)

      if (particles_stucked(i) >= threshold) then 
        ! stucked will get new prime position von swarm best 
        dv(:,i) = dv(:, fminloc)
        bestdesigns(:,i) =  bestdesigns(:,fminloc)
        ! reset stuck counter
        particles_stucked(i) = 0 
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



  subroutine  show_iteration_number (step, max_retries)

    !! Shows user info about result of a single iteration 

    integer, intent(in)          :: step, max_retries
    character (1) :: s1

    write(*,'(3x,I5,A1)', advance ='no') step, ':'

    if (max_retries == 0) then 
      write (*,'(1x,2x)',    advance ='no') 
    else
      write (s1,'(I1)') max_retries
      call print_colored (COLOR_NOTE,  ' r'//s1)
    end if


  end subroutine  show_iteration_number



  subroutine  show_iteration_result (radius, fmin, improved)

    !! Shows user info about result of a single iteration 

    double precision, intent(in)  :: radius ,fmin 
    logical, intent(in)           :: improved
    character(25)                 :: outstring

    write (outstring,'(ES9.1)') radius
    if (improved) then 
      call  print_colored (COLOR_NORMAL, trim(outstring))
    else
      call  print_colored (COLOR_NOTE,   trim(outstring))
    end if

    write (outstring,'(SP, 3x, F9.5,A1)') (1.0d0 - fmin) * 100.d0, '%'
    if (improved) then 
      call  print_colored (COLOR_GOOD, trim(outstring))
    else 
      call  print_colored (COLOR_NOTE, trim(outstring))
    end if

  end subroutine  show_iteration_result



  subroutine  show_particles_info (overall_best, personal_best, particles_stucked, objval)
    
    !! Shows user info about sucess of a single particle

    use eval, only : OBJ_XFOIL_FAIL, OBJ_GEO_FAIL

    double precision, intent(in)  :: overall_best
    double precision, dimension (:), intent(in)  :: personal_best, particles_stucked, objval
    integer       :: color, i, ibest 
    character (1) :: sign 

    call print_colored (COLOR_NOTE, ' ')

    ibest = minloc(objval,1)

    do i = 1, size(objval)
      if (objval(i) < overall_best .and. i == ibest) then 
        color = COLOR_GOOD                           ! better then current best
        sign  = '+'
      else if (objval(i) == OBJ_XFOIL_FAIL) then     
        color = COLOR_ERROR                          ! no xfoil convergence
        if (particles_stucked(i) > 5) then           ! ... and stcuked for a while 
          sign = '#'
        else 
          sign = 'x'
        end if 
      else if (objval(i) < personal_best(i)) then 
        color = COLOR_NOTE                           ! best of particle up to now
        sign  = '+'
      else if (objval(i) >= OBJ_GEO_FAIL) then 
        color = COLOR_NOTE                           ! no valid design 
        sign  = ' '
      else  
        color = COLOR_NOTE                         ! no improvement
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