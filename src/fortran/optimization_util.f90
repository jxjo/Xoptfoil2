! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2025 Jochen Guenzel

module optimization_util

  ! Optimization helper routines

  use os_util
  use commons,          only: show_details
  use print_util
  use string_util,      only : stri, strf

  implicit none
  private

  public :: delete_run_control, update_run_control, reset_run_control
  public :: design_radius
  public :: initial_designs, init_random_seed, initial_velocities
  public :: clip_velocity
  public :: write_history, write_history_header
  public :: dump_design
  public :: stop_requested


  contains

  subroutine init_random_seed()

  !----------------------------------------------------------------------------
  !! Initializes a random seed (subroutine from gcc.gnu.org)
  !----------------------------------------------------------------------------

  ! For ifort compatibility

#ifdef intel_compilers
  use ifport, only : getpid  
#endif

  integer, dimension(:), allocatable :: myseed
  integer :: i, n, un, istat, dt(8), pid, t(2), s
  integer(8) :: count, tms
  
  call random_seed(size = n)
  allocate(myseed(n))

  ! First try if the OS provides a random number generator

  un = 18
  open(newunit=un, file="/dev/urandom", access="stream",                       &
       form="unformatted", action="read", status="old", iostat=istat)

  if (istat == 0) then

     read(un) myseed
     close(un)

  else

     ! Fallback to XOR:ing the current time and pid. The PID is
     ! useful in case one launches multiple instances of the same
     ! program in parallel.

     call system_clock(count)
     if (count /= 0) then
        t = transfer(count, t)
     else
        call date_and_time(values=dt)
        tms = (dt(1) - 1970) * 365_8 * 24 * 60 * 60 * 1000 &
             + dt(2) * 31_8 * 24 * 60 * 60 * 1000 &
             + dt(3) * 24 * 60 * 60 * 60 * 1000 &
             + dt(5) * 60 * 60 * 1000 &
             + dt(6) * 60 * 1000 + dt(7) * 1000 &
             + dt(8)
        t = transfer(tms, t)
     end if

     s = ieor(t(1), t(2))
     pid = getpid() + 1099279 ! Add a prime
     s = ieor(s, pid)
     if (n >= 3) then
        myseed(1) = t(1) + 36269
        myseed(2) = t(2) + 72551
        myseed(3) = pid
        if (n > 3) then
           myseed(4:) = s + 37 * (/ (i, i = 0, n - 4) /)
        end if
     else
        myseed = s + 37 * (/ (i, i = 0, n - 1 ) /)
     end if

  end if

  call random_seed(put=myseed)
  deallocate(myseed)

end subroutine init_random_seed



  subroutine initial_designs (dv_0, dv_initial_perturb, dv, objval)

    !----------------------------------------------------------------------------
    !! Creates initial designs and tries to make them feasible 
    !! dv_0:    design variables of design 0 (should be seed airfoil) 
    !! dv:      initial design of matrix members and their dv
    !! objval:  objective function value of these initiial designs 1.0 ... x.0  
    !----------------------------------------------------------------------------

    use math_util,  only : clip
    use eval,       only : OBJ_XFOIL_FAIL, eval_design_is_valid

    double precision, intent(in)    :: dv_0 (:), dv_initial_perturb (:)
    double precision, intent(inout) :: dv (:,:)
    double precision, intent(inout) :: objval (:)

    integer                       :: i, pop, ndv, initcount, fevals, max_attempts
    logical, allocatable          :: design_is_valid (:) 
    double precision, allocatable :: dv_vector (:), dv_delta (:), perturb (:), penalty (:)
    character (:), allocatable    :: text

    ndv = size(dv,1)
    pop = size(dv,2)

    max_attempts = 2000

    allocate (dv_vector(ndv))
    allocate (design_is_valid(pop))
    allocate (penalty(pop))

    design_is_valid = .false.
    penalty  = 0d0

    fevals = 1 

    text = 'Generate '//stri(pop)//' initial random designs satisfying geometry constraints ... '
    ! with max '//stri(max_attempts)//' attempts'
    call print_action (text, no_crlf=.true.)

    ! take dv_0 as initial for the first particle

    dv(:,1) = dv_0
    design_is_valid (1) = .true. 
    objval (1) = 1.0d0                            ! equals seed,equals 1.0

    ! find random initial feasible designs for the rest of the gang 

    !$OMP parallel do private(initcount, dv_vector, dv_delta, perturb)

    do i = 2, pop

      initcount = 0
      perturb             = dv_initial_perturb

      ! Take a number of tries to fix infeasible designs

      do while ((initcount <= max_attempts) .and. (.not. design_is_valid(i)))

        call random_number(dv_vector)

        ! decrease perturb quadratically as attempts increase (stays high initially, drops faster near end)
        perturb = dv_initial_perturb * (1.0d0 - dble(initcount) / dble(max_attempts))**2

        ! init values will be random delta to dv_0 scaled by initial_perturb 
        dv_delta = (dv_vector - 0.5d0) * perturb
        dv (:,i) = dv_0 + dv_delta
        dv (:,i) = clip (dv (:,i), 0.0d0, 1.0d0)      ! ensure in bounds

        ! evaluate airfoil geometry and check if it doesn't hurt geometry constraints 
        call eval_design_is_valid (dv(:,i), design_is_valid(i), penalty(i))
    
        initcount = initcount + 1

        !$omp atomic 
        fevals = fevals + 1
        
      end do

      ! initial objective value for particle 

      if (.not. design_is_valid(i)) then              ! no design found fallback to dv_0  
        dv(:,i) = dv_0
        objval (i) = 1.0d0                            ! equals seed,equals 1.0 
        penalty(i) = 99d0 ! dummy
      else                                            ! geometric valid design found 
        objval (i) = 1d0 + penalty(i)                  
      end if 

    end do

    !$omp end parallel do

    if (show_details) then 
      call assess_and_show_results (design_is_valid, fevals)
    end if

  end subroutine initial_designs


  
  function clip_velocity (dv, max_speed, vel) result (new_vel)
    
    !! Checks if a particle having dv moving with velocity
    !!    violates speed limit 
    !!    would violate boundaries 0..1 
    !! Repairs violations by reducing vel 

    double precision, intent(in)    :: dv(:)    
    double precision, intent(in)    :: max_speed
    double precision, intent(in)    :: vel(:)  
    
    double precision, allocatable   :: new_vel(:)

    integer           :: idv
    double precision  :: new_pos, new_speed

    new_vel = vel

    do idv = 1, size (dv)  
      
      ! limit speed if necessary - for each dv 
      new_speed = vel(idv)
      if (abs(new_speed) > max_speed) then 
        new_speed = max_speed * new_speed/abs(new_speed)
      end if  

      ! check if new position would violate boundaries
      new_pos = dv(idv) + new_speed
      if (new_pos < 0d0) then
        new_speed = -dv(idv) / 2d0                 ! new pos half way to 0.0 
      elseif (new_pos > 1d0) then
        new_speed = (1d0 - dv(idv)) / 2d0          ! new pos half way to 1.0  
      end if

      new_vel(idv) = new_speed
    end do

  end function clip_velocity



  function initial_velocities (dv, max_speed) result (vel)

    !-----------------------------------------------------------------------------
    !! Generates random initial velocities that ensure particles' first step
    !! stays in valid geometry regions. For each particle, generates random
    !! velocity and tests if dv + vel is feasible. If not, retries with new
    !! random velocity (up to max_attempts). If still failing, tries smaller
    !! magnitude velocities. Sets vel = 0 as last resort.
    !-----------------------------------------------------------------------------

    use math_util, only : clip

    double precision, intent(in)    :: dv(:,:)
    double precision, intent(in)    :: max_speed

    integer, parameter   :: max_attempts = 100
    double precision, allocatable :: vel(:,:)
    integer          :: i, pop, ndv
    character (:), allocatable :: text
    double precision :: speed_scale

    ! limit initial speed to get early first improvements,
    ! but still allow some exploration (can be reduced in later iterations)
    speed_scale = 0.05d0

    pop = size(dv, 2)
    ndv = size(dv, 1)
    allocate(vel(ndv, pop))

    text = 'Generate '//stri(pop)//' initial velocities ... '
    call print_action (text)

    do i = 1, pop
      call random_number(vel(:,i))
      vel(:,i) = max_speed * (vel(:,i) - 0.5d0) * speed_scale
      vel(:,i) = clip_velocity (dv(:,i), max_speed, vel(:,i))  
    end do  

  end function initial_velocities



  subroutine ensure_valid_initial_velocities(dv, vel, max_speed)

    !-----------------------------------------------------------------------------
    !! Generates random initial velocities that ensure particles' first step
    !! stays in valid geometry regions. For each particle, generates random
    !! velocity and tests if dv + vel is feasible. If not, retries with new
    !! random velocity (up to max_attempts). If still failing, tries smaller
    !! magnitude velocities. Sets vel = 0 as last resort.
    !-----------------------------------------------------------------------------

    use eval, only : eval_design_is_valid

    double precision, intent(in)    :: dv(:,:)
    double precision, intent(out)   :: vel(:,:)
    double precision, intent(in)    :: max_speed

    integer, parameter   :: max_attempts = 100
    double precision, dimension(3)  :: speed_scales = [1d0, 0.5d0,  0.1d0]  
    double precision, allocatable :: vel_trial(:)
    integer          :: i, ndv, pop, attempt, nfailed, k
    double precision :: speed_scale
    logical          :: found_valid
    character (:), allocatable :: text

    ndv = size(dv, 1)
    pop = size(dv, 2)
    allocate(vel_trial(ndv))
    nfailed = 0

    text = 'Generate '//stri(pop)//' initial velocities satisfying geometry constraints ... '
    call print_action (text)

    !$omp parallel do private(vel_trial, attempt, speed_scale, found_valid) &
    !$omp& reduction(+:nfailed)
    
    do i = 1, pop
      
      found_valid = .false.

      do k = 1, size(speed_scales)
        speed_scale = speed_scales(k)

        do attempt = 1, max_attempts
          call random_number(vel_trial)
          vel_trial = max_speed * speed_scale * (vel_trial - 0.5d0)
          
          vel_trial = clip_velocity (dv(:,i), max_speed, vel_trial)  

          call eval_design_is_valid (dv(:,i) + vel_trial, found_valid)
          if (found_valid) then
            vel(:,i) = vel_trial
            exit
          end if
        end do

        if (found_valid) exit
      end do

      ! Last resort: zero velocity (particle starts stationary)
      if (.not. found_valid) then
        vel(:,i) = 0d0
        nfailed = nfailed + 1
      end if
      
    end do  

    !$omp end parallel do

    ! Report generation results
    if (nfailed > 0) then
      text = stri(nfailed)//" of "//stri(pop)//" particles got zero velocity (will start stationary)"
      call print_warning (text)
    end if

  end subroutine ensure_valid_initial_velocities



  subroutine  assess_and_show_results (design_is_valid, fevals)

    !! Shows user info about result of initial design evaluation 

    use eval_constraints,   only: penalty_stats_print_table, penalty_stats_init

    logical, allocatable, intent(in)  :: design_is_valid (:) 
    integer, intent(in)  :: fevals
    integer       :: i, pop, qual, nvalid, intent
    double precision :: nvalid_percent
    character (1) :: sign 

    if (.not. show_details) return 

    pop = size(design_is_valid)

    ! asses result 

    nvalid = count(design_is_valid)
    nvalid_percent = dble(nvalid) / dble(pop)
    if (nvalid_percent > 0.90) then 
      qual = Q_GOOD
    elseif (nvalid_percent > 0.3) then
      qual = Q_OK
    elseif (nvalid_percent > 0.1) then
      qual = Q_BAD
    else
      qual = Q_PROBLEM
    end if 

    if (qual == Q_GOOD) then 
      call print_colored_rating(qual)
      print * 
      call penalty_stats_init ()
      return
    end if
    
    ! detailed info if result is not good

    intent = 5 
    print *
    call print_colored (COLOR_NOTE, repeat (" ",intent)//"Total "//stri(fevals)//" evaluations: ")

    do i = 1, pop
      if (design_is_valid(i)) then 
        sign  = '+'
      else  
        sign  = '-'
      end if 
      call print_colored (COLOR_NOTE, sign)     
    end do 
    
    call print_colored (COLOR_NOTE, " ")  
    call print_colored_rating (qual)
    print * 

    ! violation statistics 

    call penalty_stats_print_table (intent)

    ! final remark 

    if (qual >= Q_BAD) then 
      call print_note ("Not enough valid designs - decrease 'inital_perturb'.", 5)
    end if 
    
    call penalty_stats_init ()

  end subroutine assess_and_show_results



  function design_radius(dv)

    !----------------------------------------------------------------------------
    !! Computes max radius of designs (used for evaluating convergence)
    !----------------------------------------------------------------------------

    double precision, dimension(:,:), intent(in) :: dv
    double precision design_radius

    integer :: i, ndesigns
    double precision, dimension(size(dv,1)) :: design_centroid
    double precision :: radius

    ! Compute centroid of designs

    ndesigns = size(dv,2)
    design_centroid(:) = 0.d0
    do i = 1, ndesigns
      design_centroid = design_centroid + dv(:,i)
    end do
    design_centroid = design_centroid / dble(ndesigns)

    ! Compute max design radius

    design_radius = 0.d0
    do i = 1, ndesigns
      radius = norm2(dv(:,i) - design_centroid)
      if (radius > design_radius) design_radius = radius
    end do

  end function




  subroutine dump_design (filename, filestat, variables, counter)

    !! Writes design variables to file

    character(*), intent(in) :: filename, filestat
    double precision, dimension(:), intent(in) :: variables
    integer, intent(in) :: counter

    integer :: iunit
    integer :: nvars, i 

    nvars = size(variables,1)

    ! Open the file and write to it if requested

    if (trim(filestat) == 'new') then
      open(newunit=iunit, file=filename, status='replace')
      write(iunit,'(A)') 'Number of variables: '//stri(nvars)
    else
      open(newunit=iunit, file=filename, status='old', position='append')
    end if

    ! Write iteration number and the design variables to file

    write(iunit,'(A)') 'Design number '//stri(counter)
    do i = 1, nvars
      write(iunit,'(es25.16)') variables(i)
    end do

  ! Close the file 

    close(iunit)

  end subroutine dump_design



  subroutine reset_run_control ()

    !! Write empty run_control file - will update file date

    integer :: iunit
    open(newunit=iunit, file='run_control', status='replace')
    close(iunit)
  end subroutine 


  subroutine update_run_control (iteration, designcounter,fmin)

    !! update run_control file with info about current optimization state

    integer, intent(in)           :: iteration, designcounter
    double precision, intent(in)  :: fmin

    integer :: iunit

    if (stop_requested()) return      ! if stop command already issued, do not update file 

    open(newunit=iunit, file='run_control', status='replace')

    write (iunit,'("!run-info; step: ",A,"; design: ",A,"; fmin: ", F10.7)') stri(iteration), stri(designcounter), fmin

    close (iunit)

  end subroutine 



  subroutine delete_run_control()

    !! Delete run_control file 

    logical :: exists
    integer :: io, stat
    inquire(file="run_control", exist=exists)
    if (exists) then
      open(file="run_control", newunit=io, iostat=stat)
      if (stat == 0) close(io, status="delete", iostat=stat)
    end if
  end subroutine 


  function stop_requested () result (is_requested)

    !! returns .true. if there is a stop command in 'run_control' 

    character(80) :: buffer
    integer :: rcunit, ioerr
    logical :: is_requested

    is_requested = .false.

    open(newunit=rcunit, file='run_control', status='old', iostat=ioerr)
    if (ioerr /= 0) return

    do
      read(rcunit, '(A)', iostat=ioerr) buffer
      if (ioerr /= 0) exit                        ! EOF or read error
      if (trim(buffer) == 'stop') then
        is_requested = .true.
        exit                                      ! no need to read further
      end if
    end do

    close(rcunit)

  end function 



  subroutine write_history_header (filename)

    !! write csv header of op points data 

    character(:), allocatable, intent(in)   :: filename

    !character (:), intent(in)     :: histfile
    integer           :: iunit

    open(newunit=iunit, file=filename, status='replace')
    write (iunit,'(A)') '  Iter;Design;  Objective;  % Improve; Design rad'
    close (iunit)

  end subroutine


  subroutine  write_history (filename, step, new_design, designcounter, radius, fmin)

    !! write iteration result to history file 'iunit' during optimization

    character(:), allocatable, intent(in)   :: filename
    integer, intent(in)           :: step, designcounter 
    logical, intent(in)           :: new_design
    double precision, intent(in)  :: radius ,fmin
    double precision  :: relfmin
    integer           :: iunit, ioerr

    open(newunit=iunit, file=filename, status='old', position='append', iostat=ioerr)
    if (ioerr /= 0) call my_stop ('Cannot open history file '//trim(filename))

    relfmin = (1.0d0 - fmin) * 100.d0
    if (new_design) then 
      write(iunit,'(I6,";",I6, 3(";", F11.7))') step, designcounter, fmin, relfmin, radius
    else 
      write(iunit,'(I6,";",A6, 3(";", F11.7))') step, ''           , fmin, relfmin, radius
    end if 
    close (iunit)

  end subroutine  write_history



end module optimization_util
