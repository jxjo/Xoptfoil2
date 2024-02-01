! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2024 Jochen Guenzel

module optimization_util

  use os_util
  use commons,        only: show_details
  use print_util

! Module containing optimization routines

  implicit none

  contains


  subroutine init_random_seed()

  !! Initializes a random seed (subroutine from gcc.gnu.org)

  ! #todo still needed? 

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



subroutine initial_designs (dv_0, dv_initial_perturb, max_attempts, dv, objval)

  !----------------------------------------------------------------------------
  !! Creates initial designs and tries to make them feasible 
  !! dv_0:    design variables of design 0 (should be seed airfoil) 
  !! dv:      initial design of matrix members and their dv
  !! objval:  objective function value of these initiial designs 1.0 ... x.0  
  !----------------------------------------------------------------------------

  use eval,       only: is_design_valid

  double precision, intent(in)    :: dv_0 (:), dv_initial_perturb (:)
  integer, intent(in)             :: max_attempts
  double precision, intent(inout) :: dv (:,:)
  double precision, intent(inout) :: objval (:)

  integer                       :: i, j, pop, ndv, initcount, fevals
  logical, allocatable          :: design_is_valid (:) 
  double precision, allocatable :: dv_vector (:), dv_delta (:)
  character (:), allocatable    :: text

  ndv = size(dv,1)
  pop = size(dv,2)

  allocate (dv_vector(ndv))
  allocate (design_is_valid(pop))
  design_is_valid = .false.

  fevals = 1 

  text = 'Generate '//stri(pop)//' initial random designs satisfying geometry constraints'
  ! with max '//stri(max_attempts)//' attempts'
  call print_action (text, show_details)

  ! take dv_0 as initial for the first particle

  dv(:,1) = dv_0
  design_is_valid (1) = .true. 

  ! find random initial feasible designs for the rest of the gang 

!$OMP parallel do private(j, initcount, dv_vector)

  do i = 2, pop

    initcount = 0

    ! Take a number of tries to fix infeasible designs

    do while ((initcount <= max_attempts) .and. (.not. design_is_valid(i)))

      call random_number(dv_vector)

      ! init values will be random delta to dv_0 scaled by initial_perturb 
      dv_delta = (dv_vector - 0.5d0) * dv_initial_perturb
      do j = 1, ndv
        dv(j,i) = dv_0(j) + dv_delta (j) 
        dv(j,i) = max (dv(j,i), 0.01d0)
        dv(j,i) = min (dv(j,i), 0.99d0)
      end do 

      ! evaluate airfoil geometry and check if it doesn't hurt geometry constraints 

      design_is_valid(i) = is_design_valid (dv(:,i))
  
      initcount = initcount + 1

!$omp critical
      fevals = fevals + 1
!$omp end critical
    end do

    if (.not. design_is_valid(i)) then              ! no design found fallback to dv_0  
      dv(:,i) = dv_0
      objval (i) = 1.0d0                            ! equals seed,equals 1.0 
    else                                            ! geometric valid design found 
      objval (i) = 1.1d0                            ! obj a little worse 
    end if 

  end do

!$omp end parallel do

  call assess_and_show_results (design_is_valid, fevals)


end subroutine initial_designs



subroutine  assess_and_show_results (design_is_valid, fevals)

  !! Shows user info about result of initial design evaluation 

  use eval_constraints,   only: violation_stats_print, violation_stats_reset

  logical, allocatable, intent(in)  :: design_is_valid (:) 
  integer, intent(in)  :: fevals
  integer       :: color, i, pop, qual, nvalid, intent
  character (1) :: sign 
  character (:), allocatable :: text 

  if (.not. show_details) return 

  intent = 5 
  call print_colored (COLOR_NOTE, repeat (" ",intent)//"Total "//stri(fevals)//" evaluations: ")

  pop = size(design_is_valid)
  nvalid = 0 

  ! print result for each member 

  do i = 1, pop
    if (design_is_valid(i)) then 
      nvalid = nvalid + 1
      color = COLOR_NOTE                          
      sign  = '+'
    else  
      color = COLOR_NOTE                         
      sign  = '-'
    end if 
    call print_colored (color, sign)     
  end do 

  ! asses result 

  if (1d0 * nvalid/pop  > 0.95) then 
    qual = Q_GOOD
    text = "Ok"
  elseif (1d0 * nvalid/pop  > 0.75) then
    qual = Q_GOOD
    text = "Ok"
  elseif (1d0 * nvalid/pop  > 0.2) then
    qual = Q_BAD
    text = "Not good"
  else
    qual = Q_PROBLEM
    text = "Problem"
  end if 
  
  call print_colored_s (qual, " "//text)
  print * 

  ! violation statistics 

  call violation_stats_print (intent)
  call violation_stats_reset ()

  ! final remark 

  if (qual >= Q_BAD) then 
    call print_note ("Not enoughs valid designs - decrease 'inital_perturb'.", 5)
  end if 


  

end subroutine assess_and_show_results


function design_radius(dv)

  !----------------------------------------------------------------------------
  !! Computes max radius of designs (used for evaluating convergence)
  !----------------------------------------------------------------------------

  use math_deps, only : norm_2

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
    radius = norm_2(dv(:,i) - design_centroid)
    if (radius > design_radius) design_radius = radius
  end do

end function




subroutine bubble_sort(dv, objvals)

  !! Sorts a set of designs according to their objective function value

  double precision, dimension(:,:), intent(inout) :: dv
  double precision, dimension(:), intent(inout) :: objvals

  double precision, dimension(size(dv,1),size(dv,2)) :: tempdv
  double precision, dimension(size(dv,2)) :: tempvals
  integer, dimension(size(dv,2)) :: finalorder, temporder
  integer :: nvars, ndesigns, i, sortcounter
  logical :: sorted

  nvars = size(dv,1)
  ndesigns = size(dv,2)

  ! Set up indexing array

  do i = 1, ndesigns
    finalorder(i) = i
  end do
  temporder = finalorder

  ! Bubble sorting algorithm

  sorted = .false.
  tempvals = objvals
  do while (.not. sorted)

    sortcounter = 0
    do i = 1, ndesigns - 1
      if (objvals(i+1) < objvals(i)) then

        ! Flip the order of these elements. temp arrays are to preserve values.

        tempvals(i) = objvals(i+1)
        tempvals(i+1) = objvals(i)
        temporder(i) = finalorder(i+1)
        temporder(i+1) = finalorder(i)
        finalorder(i) = temporder(i)
        finalorder(i+1) = temporder(i+1)
        objvals(i) = tempvals(i)
        objvals(i+1) = tempvals(i+1)
        sortcounter = sortcounter + 1

      end if
    end do
    if (sortcounter == 0) sorted = .true.
    
  end do

  ! Use indexing array to rearrange order of designs

  do i = 1, ndesigns
    tempdv(:,i) = dv(:,finalorder(i))
  end do
  dv = tempdv

end subroutine bubble_sort




subroutine pop_double_vector(vector, nitems, popidx)

  !! Pops item out of a vetor.  Note: doesn't actually change size of vector, just
  !! shuffles data so that the first nitems-1 entries represent the new vector.

  double precision, dimension(:), intent(inout) :: vector
  integer, intent(in) :: nitems, popidx

  integer :: i
  double precision, dimension(size(vector,1)) :: tempvector

  tempvector = vector

  ! Populate output vector

  do i = 1, nitems-1
    if (i < popidx) then
      vector(i) = tempvector(i)
    else
      vector(i) = tempvector(i+1)
    end if
  end do

end subroutine pop_double_vector




subroutine pop_integer_vector(vector, nitems, popidx)

  !! Pops item out of a vetor.  Note: doesn't actually change size of vector, just
  !! shuffles data so that the first nitems-1 entries represent the new vector.

  integer, dimension(:), intent(inout) :: vector
  integer, intent(in) :: nitems, popidx

  integer :: i
  integer, dimension(size(vector,1)) :: tempvector

  tempvector = vector

  ! Populate output vector

  do i = 1, nitems-1
    if (i < popidx) then
      vector(i) = tempvector(i)
    else
      vector(i) = tempvector(i+1)
    end if
  end do

end subroutine pop_integer_vector




subroutine dump_design (filename, filestat, variables, counter)

  !! Writes design variables to file

  character(*), intent(in) :: filename, filestat
  double precision, dimension(:), intent(in) :: variables
  integer, intent(in) :: counter

  integer, save :: iunit
  integer :: nvars, i 

  nvars = size(variables,1)
  iunit = 17

  ! Open the file and write to it if requested

  if (trim(filestat) == 'new') then
    open(unit=iunit, file=filename, status='replace')
    write(iunit,'(A)') 'Number of variables: '//stri(nvars)
  else
    open(unit=iunit, file=filename, status='old', position='append')
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
  iunit = 23
  open(unit=iunit, file='run_control', status='replace')
  close(iunit)
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

  rcunit = 18
  open(unit=rcunit, file='run_control', status='old', iostat=ioerr, err=501)
  if (ioerr == 0) then
    do while (1 .eq. 1)
      read(rcunit,'(A)',end=501) buffer       ! normal end will jump to 501 
      if (trim(buffer) == "stop")  then
        is_requested = .true. 
      end if
    end do
  else
    return 
  end if
   
  write(*,*) "Warning: error encountered while reading run_control. Skipping."
  return

  501 close(rcunit)

end function 



subroutine write_history_header (filename)

  !! write csv header of op points data 

  character(:), allocatable, intent(in)   :: filename

  !character (:), intent(in)     :: histfile
  integer           :: iunit

  iunit = 17
  open (unit=iunit, file=filename, status='replace')
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

  open (unit=iunit, file=filename, status='old', position='append', iostat=ioerr)
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
