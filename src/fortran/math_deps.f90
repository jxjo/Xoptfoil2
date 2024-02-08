! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2024 Jochen Guenzel

module math_deps

  ! Contains various math functions and numerical methods

  implicit none

  contains

  function diff_1D (x) 

    !----------------------------------------------------------------------------
    !! difference of 1d array elements
    !----------------------------------------------------------------------------

    double precision, dimension(:), intent(in)  :: x
    double precision, dimension(size(x)-1)      :: diff_1D 
    integer :: i, n 

    n = size(x) 
    if (n > 1) then 
      do i = 1, n-1
        diff_1D (i) = x(i+1) - x(i)
      end do 
    end if 
  end function 


  function linspace (xstart, xend, num) result (y)

    !----------------------------------------------------------------------------
    !! Return num evenly spaced numbers over a interval start-stop 
    !----------------------------------------------------------------------------

    double precision, intent(in)  :: xstart, xend
    integer, intent(in)           :: num

    double precision, allocatable :: y(:)
    double precision              :: delta  
    integer                       :: i, n
    n = max(2, num)
    allocate (y(n))
    delta = (xend - xstart) / (n-1) 

    do i = 1, n
      y(i) = xstart + (i-1) * delta 
    end do 

    y(n) = xend                            ! avoid numerical issues  

  end function 




!=============================================================================80
!
! Function to get x = inv(A)*C using gaussian elimination
!
!=============================================================================80
function lmult(A,C) result(X)

  double precision, dimension(:,:), intent(in) :: A
  double precision, dimension(:), intent(in) :: C
  double precision, dimension(size(C,1)) :: X
  double precision, dimension(size(C,1),size(C,1)+1) :: Q
  integer :: N, i, j, R
  double precision :: elim, pivot, rscale, rsum, eps
  eps = 1D-16

! Initialize

  N = size(C,1)
  if (size(A,1) /= N .or. size(A,2) /= N) then
    write(*,*)
    write(*,*) 'Error: for A*X = C and size(C) = Nx1, size(A) must be NxN'
    write(*,*)
    stop
  end if
  X(:) =  0.d0
  Q(:,1:N) = A(:,:)
  Q(:,N+1) = C(:)

! Gaussian elimination loop to put in upper triangular form

  do R = 1, N-1
    pivot = Q(R,R)
    do i = R+1, N
      elim = Q(i,R)
      if (abs(elim) > eps) then
        rscale = elim/pivot
        Q(i,:) = Q(i,:) - rscale*Q(R,:)
      end if
    end do
  end do

! Solution loop

  do i = N, 1, -1
    rsum = Q(i,N+1)
    do j = N, i+1, -1
      if (abs(Q(i,j)) > eps) rsum = rsum - Q(i,j)*X(j)
    end do
    if (Q(i,i) == 0) then
      write(*,*)
      write(*,*) 'Error in lmult: singular matrix.'
      stop
    else
      X(i) = rsum/Q(i,i)
    end if
  end do

end function lmult

!=============================================================================80
!
! Normal distribution function, used for small spacing at ends and greater in
! the middle
!
!=============================================================================80
function normal_dist(x, sig, mu) result(val)

  double precision, intent(in) :: x, sig, mu
  double precision val, pi

  pi = acos(-1.d0)
  val = 1.d0/(sig*sqrt(2.d0*pi))*exp(-(x-mu)**2.d0/(2.d0*sig**2.d0))

end function normal_dist

!=============================================================================80
!
! Vector norm (since not all compilers may include it by default)
!
!=============================================================================80
function norm_2(vector) result(val)

  double precision, dimension(:), intent(in) :: vector
  double precision :: val
  integer :: nelem, i

! Determine size

  nelem = size(vector)

! Get vector norm

  val = 0.d0
  do i = 1, nelem
    val = val + vector(i)**2.d0
  end do
  val = sqrt(val)

end function norm_2


function norm2p (x,y) result(val)

  !! norm2 of two coordinates (for convenience) 

  double precision, intent(in) :: x, y
  double precision :: val

  val = sqrt(x**2 + y**2)

end function 



!=============================================================================80
!
! Interpolates a vector y with original coordinates x to a new set of
! coordinates xnew
!
!=============================================================================80
subroutine interp_vector(x, y, xnew, ynew)

  double precision, dimension(:), intent(in) :: x, y, xnew
  double precision, dimension(:), intent(inout) :: ynew

  logical :: isbtwn
  integer :: i, pt1, npt, nptnew

  npt = size(x,1)
  nptnew = size(xnew,1)

  pt1 = 1
  do i = 1, nptnew

!   Find interpolants

    isbtwn = .false.
    do while (.not. isbtwn .and. (pt1 < npt))
      isbtwn = between(x(pt1), xnew(i), x(pt1+1))
      if (.not. isbtwn) then
        pt1 = pt1 + 1
        if (pt1 == npt) then
          write(*,*)
          write(*,*) 'Warning: could not find interpolants.'
          write(*,*) 'x: ', xnew(i), 'xmax: ', x(npt)
          stop
        end if
      end if
    end do

!   Interpolate points

    ynew(i) = interp1(x(pt1), x(pt1+1), xnew(i), y(pt1), y(pt1+1))

  end do

end subroutine interp_vector

!=============================================================================80
!
! Interpolates between two points
!
!=============================================================================80
function interp1(x1, x2, x, y1, y2) result(y)

  double precision, intent(in) :: x1, x2, x, y1, y2
  double precision y

  y = y1 + (y2 - y1)*(x - x1)/(x2 - x1)

end function interp1

!=============================================================================80
!
! Determines if B is between A and C
!
!=============================================================================80
function between(A, B, C) result(test)

  double precision, intent(in) :: A, B, C
  logical test

  if ((B >= A) .and. (B <= C)) then
    test = .true.
  else
    test = .false.
  end if 

end function between

!=============================================================================80
!
! Computes curvature for a function gam(s) = x(s) + y(s)
!
!=============================================================================80
function curvature(npt, x, y)

  integer, intent(in) :: npt
  double precision, dimension(npt), intent(in) :: x, y
  double precision, dimension(npt) :: curvature

  integer :: i
  double precision, dimension(npt) :: svec
  double precision :: se, se2
  double precision :: xe, ye, xe2, ye2
  double precision :: xs, ys, xs2, ys2

  ! Airfoil length vector s 

  svec(1) = 0.d0
  do i = 2, npt
    svec(i) = svec(i-1) + sqrt((x(i)-x(i-1))**2.d0 + (y(i)-y(i-1))**2.d0)
  end do
  
! Compute first and second derivatives and curvature vector

  do i = 1, npt

    if (i == 1) then

!     Grid metric ds/de and d2s/de2

      se = derv1f(svec(i+2), svec(i+1), svec(i), 1.d0)
      se2 = derv2f(svec(i+2), svec(i+1), svec(i), 1.d0)

!     Derivatives of x and y with respect to the grid parameter e

      xe = derv1f(x(i+2), x(i+1), x(i), 1.d0)
      ye = derv1f(y(i+2), y(i+1), y(i), 1.d0)
      xe2 = derv2f(x(i+2), x(i+1), x(i), 1.d0)
      ye2 = derv2f(y(i+2), y(i+1), y(i), 1.d0)

    elseif (i == npt) then

!     Grid metric ds/de and d2s de2

      se = derv1b(svec(i-2), svec(i-1), svec(i), 1.d0)
      se2 = derv2b(svec(i-2), svec(i-1), svec(i), 1.d0)

!     Derivatives of x and y with respect to the grid parameter e

      xe = derv1b(x(i-2), x(i-1), x(i), 1.d0)
      ye = derv1b(y(i-2), y(i-1), y(i), 1.d0)
      xe2 = derv2b(x(i-2), x(i-1), x(i), 1.d0)
      ye2 = derv2b(y(i-2), y(i-1), y(i), 1.d0)
      
    else

!     Grid metric ds/de and d2s de2

      se = derv1c(svec(i+1), svec(i-1), 1.d0)
      se2 = derv2c(svec(i+1), svec(i), svec(i-1), 1.d0)

!     Derivatives of x and y with respect to the grid parameter e

      xe = derv1c(x(i+1), x(i-1), 1.d0)
      ye = derv1c(y(i+1), y(i-1), 1.d0)
      xe2 = derv2c(x(i+1), x(i), x(i-1), 1.d0)
      ye2 = derv2c(y(i+1), y(i), y(i-1), 1.d0)

    end if

!   Derivatives of x and y with respect to surface length s

    xs = 1.d0/se * xe
    ys = 1.d0/se * ye
    xs2 = 1.d0/se**2.d0 * (xe2 - se2/se*xe)
    ys2 = 1.d0/se**2.d0 * (ye2 - se2/se*ye)

!   Curvature

    curvature(i) = (xs*ys2 - ys*xs2) / (xs**2.d0 + ys**2.d0)**1.5d0

  end do

end function curvature

!=============================================================================80
!
! Forward difference approximation for first derivative (1st  order)
!
!=============================================================================80
function derv1f1(u_plus1, u, h)

  double precision, intent(in) :: u_plus1, u, h
  double precision :: derv1f1

  derv1f1 = (u_plus1 - u)/h 

end function derv1f1

!=============================================================================80
!
! Forward difference approximation for first derivative (2nd order)
!
!=============================================================================80
function derv1f(u_plus2, u_plus1, u, h)

  double precision, intent(in) :: u_plus2, u_plus1, u, h
  double precision :: derv1f

  derv1f = (-3.d0*u + 4.d0*u_plus1 - u_plus2) / (2.d0*h)

end function derv1f

!=============================================================================80
!
! Backward difference approximation for first derivative (1st order)
!
!=============================================================================80
function derv1b1(u_minus1, u, h)

  double precision, intent(in) :: u_minus1, u, h
  double precision :: derv1b1

  derv1b1 = (u - u_minus1)/h

end function derv1b1

!=============================================================================80
!
! Backward difference approximation for first derivative (2nd order)
!
!=============================================================================80
function derv1b(u_minus2, u_minus1, u, h)

  double precision, intent(in) :: u_minus2, u_minus1, u, h
  double precision :: derv1b

  derv1b = (3.d0*u - 4.d0*u_minus1 + u_minus2) / (2.d0*h)

end function derv1b

!=============================================================================80
!
! Central difference approximation for first derivative (2nd order)
!
!=============================================================================80
function derv1c(u_plus, u_minus, h)

  double precision, intent(in) :: u_plus, u_minus, h
  double precision :: derv1c

  derv1c = (u_plus - u_minus) / (2.d0*h)

end function derv1c

!=============================================================================80
!
! Forward difference approximation for second-order derivative
!
!=============================================================================80
function derv2f(u_plus2, u_plus, u, h)

  double precision, intent(in) :: u_plus2, u_plus, u, h
  double precision :: derv2f

  derv2f = (u - 2.d0*u_plus + u_plus2) / h**2.d0

end function derv2f

!=============================================================================80
!
! Backward difference approximation for second-order derivative
!
!=============================================================================80
function derv2b(u_minus2, u_minus, u, h)

  double precision, intent(in) :: u_minus2, u_minus, u, h
  double precision :: derv2b

  derv2b = (u - 2.d0*u_minus + u_minus2) / h**2.d0

end function derv2b

!=============================================================================80
!
! Central difference approximation for second-order derivative
!
!=============================================================================80
function derv2c(u_plus, u, u_minus, h)

  double precision, intent(in) :: u_plus, u, u_minus, h
  double precision :: derv2c

  derv2c = (u_plus - 2.d0*u + u_minus) / h**2.d0

end function derv2c



!=============================================================================80
!
! Generates a pseudo-random integer in the specified range
!
!=============================================================================80
function random_integer(low, high)

  integer, intent(in) :: low, high
  integer :: random_integer

  double precision :: randdble

! Generate a random number in the range (0, 1)

  call random_number(randdble)

! Scale, translate, and convert to integer

  random_integer = low + floor(randdble*dble(high - low + 1))

end function random_integer

!=============================================================================80
!
! Generates a pseudo-random double precision number in the specified range
!
!=============================================================================80
function random_double(low, high)

  double precision, intent(in) :: low, high
  double precision :: random_double

  double precision :: randdble

! Generate a random number in the range (0, 1)

  call random_number(randdble)

! Scale and translate

  random_double = low + randdble*(high - low)

end function random_double

!=============================================================================80
!
! Swaps two elements of vector
!
!=============================================================================80
subroutine swap_double(vec, idx0, idx1)

  double precision, dimension(:), intent(inout) :: vec
  integer, intent(in) :: idx0, idx1

  double precision :: t1, t2

  t1 = vec(idx0)
  t2 = vec(idx1)
  vec(idx0) = t2
  vec(idx1) = t1

end subroutine swap_double

subroutine swap_int(vec, idx0, idx1)

  integer, dimension(:), intent(inout) :: vec
  integer, intent(in) :: idx0, idx1

  integer :: t1, t2

  t1 = vec(idx0)
  t2 = vec(idx1)
  vec(idx0) = t2
  vec(idx1) = t1

end subroutine swap_int

!=============================================================================80
!
! Sorts a vector via bubble sort. Optionally records map of indices relative to
! input vector.
!
!=============================================================================80
subroutine sort_vector(vec, idxs)

  double precision, dimension(:), intent(inout) :: vec
  integer, dimension(:), intent(inout), optional :: idxs

  integer :: nelem, i, sortcounter
  logical :: sorted

! Set up indexing array

  nelem = size(vec,1)
  if (present(idxs)) then
    do i = 1, nelem
      idxs(i) = i
    end do
  end if

! Bubble sorting algorithm

  sorted = .false.
  do while (.not. sorted)

    sortcounter = 0
    do i = 1, nelem-1
      if (vec(i+1) < vec(i)) then
        call swap_double(vec, i, i+1)
        sortcounter = sortcounter + 1
        if (present(idxs)) call swap_int(idxs, i, i+1)
      end if
    end do
    if (sortcounter == 0) sorted = .true.

  end do

end subroutine sort_vector
!=============================================================================80
!
! Sorts a vector via bubble sort descending. Optionally records map of indices relative to
! input vector.
!
!=============================================================================80
subroutine sort_vector_descend (vec, idxs)

  double precision, dimension(:), intent(inout) :: vec
  integer, dimension(:), intent(inout), optional :: idxs

  integer :: nelem, i, sortcounter
  logical :: sorted

! Set up indexing array

  nelem = size(vec,1)
  if (present(idxs)) then
    do i = 1, nelem
      idxs(i) = i
    end do
  end if

! Bubble sorting algorithm

  sorted = .false.
  do while (.not. sorted)

    sortcounter = 0
    do i = 1, nelem-1
      if (vec(i+1) > vec(i)) then
        call swap_double(vec, i, i+1)
        sortcounter = sortcounter + 1
        if (present(idxs)) call swap_int(idxs, i, i+1)
      end if
    end do
    if (sortcounter == 0) sorted = .true.

  end do

end subroutine sort_vector_descend

!------------------------------------------------------------------------------
! the MEDIAN of a vector
!------------------------------------------------------------------------------
function median (vec)

  double precision :: median 

  double precision, dimension(:), intent(in) :: vec

  double precision, dimension(:), allocatable :: elements
  integer :: nelem
  
  nelem = size (vec) 
  
  If (nelem == 0 ) then 
    write (*,*) 'Error: median function called without an element'
    stop 
  elseif (nelem == 1) then 
    median = vec (1)
    return
  end if 

  elements = vec (1:nelem) 

  ! sort elements 
  call sort_vector(elements)

  ! now get the element in the middle if odd elemenets, 
  !    else the mid value of the two elements in the midle
  
  if (mod(nelem,2) == 1) then
    median = elements (nelem / 2 + 1)                                 ! odd
  else 
    median = (elements (nelem / 2)  + elements (nelem / 2 + 1)) / 2   ! even
  end if 

end function median

!------------------------------------------------------------------------------
! arccos transformation of x() 
!   Used for transformation of x-axis based on 
!   https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19850022698.pdf
!------------------------------------------------------------------------------
function transformed_arccos (x) result (x_arccos)

  double precision, dimension(:), intent(in) :: x
  double precision, dimension(size(x))       :: x_arccos

  double precision :: pi
  integer          :: i

  x_arccos = x
  pi = acos(-1.d0)

  if(x(1) == 0d0 .and. x(size(x)) == 1d0) then 
    do i = 1, size(x)
      if (x(i) < 0.d0) then         ! sanity check - nowbody knows ...
        x_arccos(i) = 0.d0
      else
        x_arccos(i) = acos(1.d0 - x(i)) * 2.d0 / pi 
      end if       
    end do
  else
    Write (*,*) 'Error: Array to transform is not x(1)= 0. and x(n)=1.'
  end if 

end function transformed_arccos




!-------------------------------------------------------------------------------------
! Central entrypoint for smoothing a polyline (x,y) being the top or bottom surface
!
! Smoothing of the polyline is done until 
!   - a certain quality (= min number of spikes) is reached
!   - no more improvment for reduction of spikes happens
!   - or max. number of iterations reached (max_iterations)
!
! Two nested loops are used for smoothing
!   The inner loop is the modified Chaikin (Corner Cut) algorithm. This loop is limited
!   to n_Chaikin_iter (typically = 5) because
!     - in each iteration the number of points will be doubled (memory / speed)
!     - there will be no real improvement ...
!   The outer loop calls Chaikin is until one of the above criteria is reached.
!
! The starting point for smoothing in the polyline is set by i_range_start.
! 
! Be careful in changin the parameters and always take a look at the result 
!    delta = y_smoothed - y_original
!------------------------------------------------------------------------------

subroutine smooth_it (show_details, spike_threshold, x, y)

  logical, intent(in) :: show_details  
  double precision, intent(in) :: spike_threshold
  double precision, dimension(:), intent(in) :: x
  double precision, dimension(:), intent(inout) :: y

  integer :: max_iterations, nspikes_target, i_range_start, i_range_end
  integer :: nspikes, istart, iend, nspikes_initial
  integer :: i, n_Chaikin_iter, n_no_improve, nspikes_best, n_no_imp_max, n_min
  double precision :: tension, sum_y_before, sum_y_after, delta_y
  character (100)     :: text_change
  
  double precision, dimension(size(x)) :: x_cos

  sum_y_before = abs(sum(y))

! Transform the x-Axis with a arccos function so that the leading area will be stretched  
! resulting in lower curvature at LE - and the rear part a little compressed
! This great approach is from Harry Morgan in his smoothing algorithm
!    see https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19850022698.pdf

  x_cos = transformed_arccos (x)

  i_range_start  = 1              ! with transformation will start smoothing now at 1
  i_range_end    = size (x)       ! ... and end
  
! Count initial value of spikes

  istart  = 1               
  iend    = size(x)

  nspikes = count_reversals (istart, iend, curv_derivative(x, y), spike_threshold)

  nspikes_target  = int(nspikes/10) ! how many curve spikes should be at the end?
  nspikes_initial = nspikes       !   Reduce by factor 5 --> not too much as smoothing become critical for surface
                                 
  tension        = 0.5d0          ! = 0.5 equals to the original Chaikin cutting distance of 0.25 
                                  !   tension will be reduced at TE

  n_Chaikin_iter = 5              ! number of iterations within Chaikin
  max_iterations = 8              ! max iterations over n_Chaikin_iter 

  nspikes_best = nspikes          ! init with current to check if there is improvement of nspikes over iterations
  n_no_improve = 0                ! iterate only until iteration with no improvements of nspikes 
  n_no_imp_max = 3                !  ... within  n_no_imp_max

  n_min = 1                       ! Minimum number of iterations
  
! Now do iteration 

  i = 0

  do while (((i < max_iterations) .and. (nspikes > nspikes_target) .and. &
            (n_no_improve < n_no_imp_max)) .or.(i < n_min))
    
    call smooth_it_Chaikin (i_range_start, i_range_end, tension, n_Chaikin_iter, x_cos, y)

    nspikes = count_reversals (istart, iend, curv_derivative(x, y), spike_threshold)

    if (nspikes < nspikes_best) then
      nspikes_best = nspikes
      n_no_improve = 0
    else
      n_no_improve = n_no_improve + 1  
    end if 

    i = i + 1

  end do

! Summarize - final info for user 

  if (show_details) then

    sum_y_after = abs(sum(y)) 
    delta_y = 100d0 * (sum_y_after - sum_y_before)/sum_y_before
    write (*,'(3x, A)', advance = 'no') "Smoothing: "
    write (text_change,'(I2, A,F8.5,A)') i, ' iterations - overall change of y values ',delta_y,'%' 

    if ( nspikes_initial == 0) then 
      write (*,'(1x, A,F4.1,A)') "No spikes found based on spike_threshold =", &
                                  spike_threshold,". "//trim(text_change)

    elseif (nspikes == 0) then 
      write (*,'(1x, A)') " All spikes removed. "//trim(text_change)

    elseif (nspikes <= nspikes_target) then 
      write (*,'(1x,A,I3,A)') "Number of spikes reduced by factor", nspikes_initial/nspikes, &
            ". "//trim(text_change)

    elseif (i >= max_iterations) then 
      write (*,'(1x,A,I2,A)') "Reached maximum iterations = ", max_iterations, &
            ". "//trim(text_change)

    elseif (n_no_improve >= n_no_imp_max) then 
      write (*,'(1x,A,I2,A)') "No further improvement within ",n_no_imp_max, &
             " iterations. " // trim(text_change)

    else 
      write (*,'(1x,A)') "Smoothing ended."          ! this shouldn't happen
    end if 

  end if

end subroutine smooth_it


!------------------------------------------------------------------------------
! smooth polyline (x,y) with a Chaikin corner cutting see
!     https://www.codeproject.com/Articles/1093960/D-Polyline-Vertex-Smoothing
!------------------------------------------------------------------------------

subroutine smooth_it_Chaikin (i_start, i_end, tension, niterations, x, y)

  integer, intent(in) :: i_start, i_end, niterations
  double precision, dimension(:), intent(inout) :: x, y
  double precision, intent(in) :: tension

  double precision, dimension(:), allocatable :: x_in, y_in
  double precision, dimension(:), allocatable :: x_out, y_out
  double precision  :: cuttingDist
  integer :: i, npoints

    
  ! the tension factor defines a scale between corner cutting distance in segment half length,
  ! i.e. between 0.05 and 0.45. The opposite corner will be cut by the inverse
  ! (i.e. 1-cutting distance) to keep symmetry.
  ! with a tension value of 0.5 this amounts to 0.25 = 1/4 and 0.75 = 3/4,
  ! the original Chaikin values

  cuttingDist = 0.05d0 + (tension*0.4d0)
  npoints = i_end - i_start + 1

  allocate (x_in(npoints))
  allocate (y_in(npoints))

  ! cut the area to smooth out of the original polyline
  x_in = x(i_start : i_end)
  y_in = y(i_start : i_end) 
  npoints = i_end - i_start + 1
  
  do i = 1, niterations
    call getSmootherChaikin(x_in, y_in, cuttingDist, x_out, y_out)
    x_in = x_out
    y_in = y_out
  end do
  
  ! replace the area to smooth in original polyline with the smoothed result
  call interp_vector(x_in, y_in, x(i_start : i_end), y(i_start : i_end))

  deallocate (x_in)
  deallocate (y_in)

end subroutine smooth_it_Chaikin

! Core smoothing function 
Subroutine getSmootherChaikin(x, y, cuttingDist, x_smooth, y_smooth)
    
  double precision, dimension(:), intent(in) :: x, y
  double precision, dimension(:), allocatable, intent(out) :: x_smooth, y_smooth
  double precision, intent(in) :: cuttingDist

  integer :: i, is, np_smooth, npt
  double precision :: cut
  
  npt       = size(x)
  np_smooth = (npt)*2

  allocate (x_smooth(np_smooth))
  allocate (y_smooth(np_smooth))

  ! always add the first point - this won't be changed
  x_smooth(1) = x(1)
  y_smooth(1) = y(1)

  cut = cuttingDist

  is = 1
  do i = 1, (npt-1)

  ! Reduce cutting distance close to trailing edge to avoid "cutting" 
  !   of curvature at TE eg at relexed airfoil 
  ! Note: shape_tpye "camb-thick" does smoothing during optimization... 
    if (i > (npt-7)) cut = cut * 0.80d0

    is = is + 1
    x_smooth(is) = (1-cut) * x(i) + cut * x(i+1)
    y_smooth(is) = (1-cut) * y(i) + cut * y(i+1)

    is = is + 1
    x_smooth(is) = cut * x(i) + (1-cut) * x(i+1)
    y_smooth(is) = cut * y(i) + (1-cut) * y(i+1)

  end do

! always add the last point so it never will be changed
  is = is + 1
  x_smooth(is) = x(npt)
  y_smooth(is) = y(npt)

end subroutine



!------------------------------------------------------------------------------
! Finds and counts reversals of a of polyline (x,y) which could be the first or
!   second derivative - only the y-values are needed
!    
! To find the "real" reversals only y value greater threshold
!    are taken to check against the change from + to -
!
! istart and iend define the range of polyline to be scanned.
!
! result_info holds a string of npoints for user entertainment 
!------------------------------------------------------------------------------
subroutine find_reversals(istart, iend, threshold, y, reversal_sign, &
                          nreversals, result_info)

  integer, intent(in) :: istart, iend
  double precision, intent(in) :: threshold
  double precision, dimension(:), intent(in) :: y
  character (1), intent(in) ::  reversal_sign 
  integer, intent(out) :: nreversals
  character (size(y)), intent(inout), optional :: result_info

  double precision :: y1
  integer :: i, is, ie, npt 

  nreversals = 0

  npt = size(y)
  is = max (istart, 1)  
  ie = min (iend, npt)            

  ! get the real reversals with curve values from + to - 
 
  y1 = 0.d0
  do i = 1, npt
    if (abs(y(i)) >= threshold) then
      if (y(i) * y1 < 0.d0) then 
        if (i >= is .and. i <= ie) then 
          nreversals = nreversals + 1
          if (present(result_info)) result_info (i:i) = reversal_sign
        end if
      end if
      y1 = y(i)
    end if
  end do

end subroutine find_reversals

!------------------------------------------------------------------------------
! Gets the number of  reversals if threshold 'threshold' is applied
!    detected on the polyline 
!------------------------------------------------------------------------------
function count_reversals (istart, iend, y, threshold)

  integer, intent(in)                         :: istart, iend
  double precision, dimension(:), intent(in)  :: y
  double precision, intent(in)                :: threshold
  integer          :: count_reversals

  call find_reversals(istart, iend, threshold, y, 'R', count_reversals)

end function count_reversals



!------------------------------------------------------------------------------
! Evaluates the min possible threshold that no more than nreversals will be
!    detected on the polyline (x,y) - only y is needed
!------------------------------------------------------------------------------
function min_threshold_for_reversals (istart, iend, y, min_threshold, max_threshold, &
                                      nreversals_to_detect)

  integer, intent(in)             :: istart, iend, nreversals_to_detect
  double precision, intent(in)    :: min_threshold, max_threshold
  double precision, dimension(:), intent(in) :: y
  
  double precision       :: min_threshold_for_reversals, threshold, decrease
  integer                :: nreversals

  if (min_threshold == 0d0) then
    write (*,*) 'Error: min_threshold must be > 0 in min_threshold_for_reversals'
    stop
  end if 

  threshold     = max_threshold
  decrease      = 0.9d0                     ! decrease threshold by 
  nreversals    = count_reversals (istart, iend, y, threshold)

  do while ((threshold >= min_threshold) .and.  &
            (nreversals <= nreversals_to_detect))

    threshold  = threshold * decrease
    nreversals = count_reversals (istart, iend, y, threshold)

  end do 

  if (nreversals > nreversals_to_detect) then
    min_threshold_for_reversals = threshold / decrease   ! to many - go delta back
  else
    min_threshold_for_reversals = threshold           ! exact match 
  end if

end function min_threshold_for_reversals




!------------------------------------------------------------------------------
! derivative of curvature of polyline (x,y)
!     ! only approx because 1st derivative of 2nd derivative is used 
!------------------------------------------------------------------------------
function curv_derivative(x, y)

  double precision, dimension(:), intent(in) :: x, y
  double precision, dimension(size(x)) :: curv_derivative

  curv_derivative = derivative1(x, curvature(size(x), x, y))
 
end function curv_derivative



!------------------------------------------------------------------------------
! get first derivative of polyline (x,y)
!     using Backward, center, forward adapted difference approximation 
!     based on "A simple finite-difference grid with non-constant intervals"
!               by HILDING SUNDQVIST
!     and on "http://web.media.mit.edu/~crtaylor/calculator.html"
!------------------------------------------------------------------------------

function derivative1(x, y)

  double precision, dimension(:), intent(in) :: x, y
  double precision, dimension(size(x)) :: derivative1
  integer :: i
  double precision :: h_minus, h, hr
!  double precision :: h_2minus, h_plus
  integer :: npt

  npt = size(x)
 
  do i = 1, npt
    if (i == 1) then                                                 ! forward
! jx-mod formula for forward is wrong! - take simple form
!      h      = x(i+1) - x(i)
!      h_plus = x(i+2) - x(i+1)
!      hr      = h_plus / h 
!      derivative1(i) = (-y(i+2) - 3.d0*hr*hr*y(i) + 4.d0*(1-hr*hr)*y(i+1))/ (h_plus * (1.d0 +hr)) 
      derivative1(i) = (y(i+1) - y(i))/ (x(i+1) - x(i)) 
    else if (i ==npt) then                                           ! backward
! jx-mod formula for backward is wrong! - take simple form
!      h_minus  = x(i) - x(i-1)
!      h_2minus = x(i-1) - x(i-2)
!      hr      = h_minus / h_2minus 
!      derivative1(i) = (3.d0*y(i) + hr*hr*y(i-2) -4.d0*(1-hr*hr)*y(i-1))/ (h_minus * (1.d0 +hr)) 
      derivative1(i) = (y(i) - y(i-1))/ (x(i) - x(i-1))
    else                                                             ! center
      h       = x(i+1) - x(i)
      h_minus = x(i) - x(i-1)
      hr      = h / h_minus 
      derivative1(i) = (y(i+1) - hr*hr*y(i-1) -(1-hr*hr)*y(i))/ (h * (1.d0 +hr)) 
    end if 
  end do

end function derivative1

!------------------------------------------------------------------------------
! get first derivative of a polyline (x,y) at point i
!     using Backward, center, forward adapted difference approximation 
!     For center the mean of backward and forward approximation is taken
!     which could be more precise ...
!------------------------------------------------------------------------------
function derivation_at_point (i, x, y)

  integer, intent(in) :: i 
  double precision, dimension(:), intent(in) :: x, y
  double precision :: derivation_at_point
  integer :: npt 

  npt = size(x)

  derivation_at_point = 0.d0
  if (i < npt) then
    ! Forward
    if (x(i+1) > x(i)) then
      derivation_at_point = (y(i+1) - y(i))/(x(i+1)-x(i))
    else
      derivation_at_point = (y(i) - y(i+1))/(x(i)-x(i+1))
    end if
  end if

  if (i > 1) then
    ! Backward
    if (x(i) > x(i-1)) then
      derivation_at_point = derivation_at_point + (y(i) - y(i-1))/(x(i)-x(i-1))
    else
      derivation_at_point = derivation_at_point + (y(i-1) - y(i))/(x(i-1)-x(i))
    end if
  end if
  
  ! center - tage mean value of both 
  if ( (i < npt) .and. (i > 1) )               &
  derivation_at_point = derivation_at_point/2.d0 

end function derivation_at_point

!------------------------------------------------------------------------------
! Interpolate a point (xnew, ynew) within a vector (x,y) - returns ynew
!------------------------------------------------------------------------------
function interp_point(x, y, xnew)

  double precision, dimension(:), intent(in) :: x, y 
  double precision, intent(in)  :: xnew
  double precision :: interp_point
  
  logical :: isbtwn
  integer :: pt1, npt

  npt = size(x,1)
  pt1 = 1

! Find interpolants
  isbtwn = .false.
  do while (.not. isbtwn .and. (pt1 < npt))
    isbtwn = between(x(pt1), xnew, x(pt1+1))
    if (.not. isbtwn) then
      pt1 = pt1 + 1
      if (pt1 == npt) then
        write(*,*)
        write(*,*) 'Warning: could not find interpolants.'
        write(*,*) 'x: ', xnew, 'xmax: ', x(npt)
        stop
      end if
    end if
  end do

! Interpolate points
  interp_point = interp1(x(pt1), x(pt1+1), xnew, y(pt1), y(pt1+1))

end function interp_point

end module math_deps
