! MIT License

module math_util

  ! Various basic math functions and numerical methods

  implicit none

  ! --- types ----------------------------------------------------------------

  type point_type
    !! Generic 2D point for curves, polygons, etc.
    double precision :: x, y
  end type point_type

  type points_type
    !! Generic 2D point array for curves, polygons, etc.
    double precision, allocatable :: x(:), y(:)
  end type points_type

  type curve_spec_type
    !! Parent type for curve specifications (Bezier, B-spline)
    !! Contains fundamental control point data
    double precision, allocatable :: px(:)  ! x coordinates of control points
    double precision, allocatable :: py(:)  ! y coordinates of control points
  end type curve_spec_type

  public :: point_type, points_type, curve_spec_type
  public :: round

  ! --- interfaces -----------------------------------------------------------

  interface clip
    module procedure clip_scalar, clip_int, clip_array
  end interface clip

  interface norm
    module procedure norm_p, norm_xy
  end interface norm

  interface round
    module procedure round_scalar, round_array
  end interface round


  contains


  pure elemental function clip_scalar (val, lo, hi) result (r)

    !----------------------------------------------------------------------------
    !! clip scalar val to [lo, hi]
    !----------------------------------------------------------------------------
  
    double precision, intent(in) :: val, lo, hi
    double precision :: r
    r = max(lo, min(hi, val))

  end function


  pure elemental function clip_int (val, lo, hi) result (i)

    !----------------------------------------------------------------------------
    !! clip scalar val to [lo, hi]
    !----------------------------------------------------------------------------
  
    integer, intent(in) :: val, lo, hi
    integer :: i
    i = max(lo, min(hi, val))

  end function


  pure function clip_array (val, lo, hi) result (r)
  
    !----------------------------------------------------------------------------
    !! clip array val(:) to [lo, hi]
    !----------------------------------------------------------------------------

    double precision, intent(in) :: val(:), lo, hi
    double precision :: r(size(val))
    r = max(lo, min(hi, val))
  end function



  recursive function diff_1D (x, n) result(diff)

    !----------------------------------------------------------------------------
    !! n-th difference of 1d array elements (like numpy.diff)
    !! n: optional order of difference (default: 1)
    !! Returns array of size size(x) - n
    !----------------------------------------------------------------------------

    double precision, dimension(:), intent(in)  :: x
    integer, intent(in), optional               :: n
    double precision, allocatable               :: diff(:)

    double precision, allocatable :: tmp(:)
    integer :: order, size_in

    ! Default to first difference
    if (present(n)) then
      order = max(1, n)
    else
      order = 1
    end if

    size_in = size(x)

    ! Check if we can compute n-th difference
    if (size_in <= order) then
      allocate(diff(0))  ! Return empty array
      return
    end if

    ! Base case: first difference
    if (order == 1) then
      allocate(diff(size_in - 1))
      diff = x(2:size_in) - x(1:size_in-1)
    else
      ! Recursive case: apply diff to the (n-1)th difference
      tmp = diff_1D(x, order - 1)
      diff = diff_1D(tmp)
    end if

  end function 


  function cumsum (x) result (cs)

    !----------------------------------------------------------------------------
    !! cumulative sum of array elements (like numpy.cumsum)
    !! Returns array of same size with cumulative sums: [x1, x1+x2, x1+x2+x3, ...]
    !----------------------------------------------------------------------------

    double precision, dimension(:), intent(in)  :: x
    double precision, dimension(size(x))        :: cs 
    integer :: i, n 

    n = size(x) 
    if (n > 0) then 
      cs(1) = x(1)
      do i = 2, n
        cs(i) = cs(i-1) + x(i)
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


  function cosine_distribution(nPoints, le_bunch, te_bunch) result(u)

    !----------------------------------------------------------------------------
    !! Returns a cosine-based distribution array of length nPoints over [0, 1]
    !!
    !! Bunching near LE is controlled by le_bunch, bunching near TE by te_bunch.
    !! Used by spline-based paneling directly as u, and by curve-based paneling
    !! (Bezier, B-Spline) as arc-length fractions that are subsequently mapped
    !! to curve parameter u via arc-length inversion.
    !!
    !! Args:
    !!   nPoints  : number of points in distribution
    !!   le_bunch : 0..1 where 1 is maximum LE bunching, 0 is minimal bunching
    !!   te_bunch : 0..1 where 1 is maximum TE bunching, 0 is no TE bunching
    !----------------------------------------------------------------------------

    integer, intent(in)          :: nPoints
    double precision, intent(in) :: le_bunch, te_bunch
    double precision, allocatable :: u(:), beta(:)
    
    double precision :: ufacStart, ufacEnd, pi, te_exponent
    
    pi = acos(-1.0d0)
    
    ! Cosine LE bunching:
    ! ufacStart=0.0 → beta starts at 0 → zero slope at LE → maximum bunching
    ! ufacStart=0.5 → beta starts at π/2 → cosine is near-linear → near-uniform
    ! le_bunch=1 → ufacStart=0.0 (max bunch); le_bunch=0 → ufacStart=0.1 (minimal)
    
    ufacStart = 0.1d0 - le_bunch * 0.1d0        ! 0.1 (no bunch) ... 0.0 (max LE bunch)
    ufacStart = max(0.0d0, min(0.5d0, ufacStart))
    ufacEnd   = 0.65d0
    
    beta = linspace(ufacStart, ufacEnd, nPoints) * pi
    u    = (1.0d0 - cos(beta)) * 0.5d0
    u    = u - u(1)                              ! shift so first point is exactly 0
    u    = u / u(nPoints)                        ! normalize to 0..1
    
    ! Trailing edge - power-law bunching on top of cosine LE distribution
    if (te_bunch > 0.0d0) then
      te_exponent = 1.0d0 + te_bunch * 0.15d0    ! 1.0 (no bunch) ... ~1.15 (max)
      u = 1.0d0 - (1.0d0 - u) ** te_exponent
    end if
    
    ! Ensure exact boundaries
    u(1)      = 0.0d0
    u(nPoints) = 1.0d0
    
  end function


  function find_closest_index (array, x) result(index)

    !----------------------------------------------------------------------------
    !! Assumes array is sorted, returns index of closest value to x.
    !----------------------------------------------------------------------------

    double precision, intent(in)    :: array(:), x
    integer                         :: index 

    integer           :: i, n
    double precision  :: before, after

    n = size(array) 

    ! get best index in x within interval j  
    do i = 1, n                             ! find interval 
      if (x <= array(i)) exit
    end do 

    if (i == 1) then 
      index = i
    else 
      before = array (i-1)
      after  = array (i)
      if ((after - x) < (x - before)) then 
        index = i
      else
        index = i - 1
      end if 
    end if 

  end function 


  function norm_p (p) result (val)

    !----------------------------------------------------------------------------
    !! 2-norm of point p
    !----------------------------------------------------------------------------

    type(point_type), intent(in) :: p
    double precision :: val

    val = sqrt(p%x**2 + p%y**2)

  end function norm_p


  function norm_xy (x, y) result (val)

    !----------------------------------------------------------------------------
    !! 2-norm of coordinates x and y
    !----------------------------------------------------------------------------

    double precision, intent(in) :: x, y
    double precision :: val

    val = sqrt(x**2 + y**2)

  end function norm_xy 


  
  function rms (x) result (val)

    !----------------------------------------------------------------------------
    !! root mean square of array x
    !----------------------------------------------------------------------------

    double precision, dimension(:), intent(in) :: x
    double precision :: val

    val = sqrt(sum(x**2) / size(x))

  end function rms



  subroutine interp_vector(x, y, xnew, ynew)

    !----------------------------------------------------------------------------
    !! Linear interpolation of a vector y with original coordinates x 
    !! to a new set of coordinates xnew having ynew
    !----------------------------------------------------------------------------

    double precision, dimension(:), intent(in) :: x, y, xnew
    double precision, dimension(:), intent(inout) :: ynew

    logical :: isbtwn
    integer :: i, pt1, npt, nptnew

    npt = size(x,1)
    nptnew = size(xnew,1)

    pt1 = 1
    do i = 1, nptnew

    ! Find interpolants

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

      ! Interpolate points

      ynew(i) = interp1(x(pt1), x(pt1+1), xnew(i), y(pt1), y(pt1+1))

    end do

  end subroutine 



  function interp1(x1, x2, x, y1, y2) result(y)

    !----------------------------------------------------------------------------
    !! Interpolates between two points
    !----------------------------------------------------------------------------

    double precision, intent(in) :: x1, x2, x, y1, y2
    double precision y

    y = y1 + (y2 - y1)*(x - x1)/(x2 - x1)

  end function 



  function between(A, B, C) result(test)

    !----------------------------------------------------------------------------
    !! Determines if B is between A and C
    !----------------------------------------------------------------------------

    double precision, intent(in) :: A, B, C
    logical test

    if ((B >= A) .and. (B <= C)) then
      test = .true.
    else
      test = .false.
    end if 

  end function 



  function random_integer(low, high)

    !----------------------------------------------------------------------------
    !! Generates a pseudo-random integer in the specified range
    !----------------------------------------------------------------------------

    integer, intent(in) :: low, high
    integer :: random_integer

    double precision :: randdble

    ! Generate a random number in the range (0, 1)

    call random_number(randdble)

    ! Scale, translate, and convert to integer

    random_integer = low + floor(randdble*dble(high - low + 1))

  end function 



  function random_double(low, high)

  !----------------------------------------------------------------------------
  !! Generates a pseudo-random double precision number in the specified range
  !----------------------------------------------------------------------------

  double precision, intent(in) :: low, high
  double precision :: random_double
  double precision :: randdble

  call random_number(randdble)
  random_double = low + randdble*(high - low)

  end function random_double




  subroutine swap_double(vec, idx0, idx1)

    !----------------------------------------------------------------------------
    !! Swaps two elements of vector
    !----------------------------------------------------------------------------

    double precision, dimension(:), intent(inout) :: vec
    integer, intent(in) :: idx0, idx1
    double precision :: t1, t2

    t1 = vec(idx0)
    t2 = vec(idx1)
    vec(idx0) = t2
    vec(idx1) = t1

  end subroutine 



  subroutine swap_int(vec, idx0, idx1)

    !----------------------------------------------------------------------------
    !! Swaps two elements of vector
    !----------------------------------------------------------------------------

    integer, dimension(:), intent(inout) :: vec
    integer, intent(in) :: idx0, idx1

    integer :: t1, t2

    t1 = vec(idx0)
    t2 = vec(idx1)
    vec(idx0) = t2
    vec(idx1) = t1

  end subroutine 



  subroutine sort_vector(vec, idxs)

    !----------------------------------------------------------------------------
    !! Sorts a vector via bubble sort. Optionally records map of indices relative to
    !! input vector.
    !----------------------------------------------------------------------------

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

  end subroutine 



  subroutine sort_vector_descend (vec, idxs)

    !----------------------------------------------------------------------------
    !! Sorts a vector via bubble sort descending. Optionally records map of indices relative to
    !! input vector.
    !----------------------------------------------------------------------------

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

  end subroutine 



  function median (vec)

    !----------------------------------------------------------------------------
    !! the MEDIAN of a vector
    !----------------------------------------------------------------------------

    double precision, dimension(:), intent(in) :: vec
    double precision :: median 
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

  end function 


  function transformed_arccos (x) result (x_arccos)

    !----------------------------------------------------------------------------
    !! arccos transformation of x() 
    !   Used for transformation of x-axis based on 
    !   https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19850022698.pdf
    !----------------------------------------------------------------------------

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

  end function 


  function tangent_angle(x, y, x_min, x_max) result (angle_deg)

    !----------------------------------------------------------------------------
    !! tangent angle in degrees of polyline (x,y) in x-range [x_min, x_max]
    !! if the tangent is falling down, the angle is positive, if rising, the angle is negative
    !   via linear regression of points in x-range
    !----------------------------------------------------------------------------

    double precision, dimension(:), intent(in)  :: x, y
    double precision, intent(in)                :: x_min, x_max
    double precision                            :: angle_deg

    double precision, allocatable :: xm(:), ym(:)
    logical, allocatable          :: mask(:)
    double precision :: sum_x, sum_y, sum_xy, sum_x2, slope, n_dp
    integer          :: n

    mask = (x >= x_min) .and. (x <= x_max)
    xm   = pack(x, mask)
    ym   = pack(y, mask)
    n    = size(xm)

    if (n < 2) then
      angle_deg = 0d0
      return
    end if

    sum_x  = sum(xm)
    sum_y  = sum(ym)
    sum_xy = sum(xm * ym)
    sum_x2 = sum(xm * xm)
    n_dp   = dble(n)

    ! --- Linear regression: y = slope*x + intercept ---
    slope     = (n_dp*sum_xy - sum_x*sum_y) / (n_dp*sum_x2 - sum_x*sum_x)

    ! --- Angle in degrees ---
    angle_deg = -atan(slope) * 180d0 / acos(-1d0)

  end function tangent_angle



  subroutine find_reversals(istart, iend, threshold, y, reversal_sign, &
                            nreversals, result_info)

    !----------------------------------------------------------------------------
    !! Finds and counts reversals of a of polyline (x,y) - only the y-values are needed
    !    
    ! To find the "real" reversals only y value greater threshold
    !    are taken to check against the change from + to -
    !
    ! istart and iend define the range of polyline to be scanned.
    !   iend = -1 means to scan to the end of the array.
    !
    ! result_info holds a string of npoints for user entertainment 
    !----------------------------------------------------------------------------
                            

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
    if (iend == -1) then 
      ie = npt
    else 
      ie = min (iend, npt)            
    end if

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

  end subroutine 


  function count_reversals (x, y, threshold, x_start, x_end, smooth) result (n)

    !----------------------------------------------------------------------------
    !! Gets the number of reversals of y for 'threshold' within x-range [x_start, x_end]
    !! A reversal is counted if it occurs within the range, even if the sign change
    !! starts from a value just before x_start
    !!
    !! smooth: optional - if true, applies moving average (~3% window) to suppress
    !!         noise-driven false sign changes
    !----------------------------------------------------------------------------

    double precision, dimension(:), intent(in) :: x, y
    double precision, intent(in) :: threshold
    double precision, intent(in), optional :: x_start, x_end
    logical, intent(in), optional :: smooth
    integer :: n

    double precision :: y_prev, xs, xe
    double precision, dimension(:), allocatable :: y_work
    integer :: i, n_window, n_half
    logical :: in_range, do_smooth

    ! Set x-range boundaries
    if (present(x_start)) then
      xs = x_start
    else
      xs = x(1)
    end if
    
    if (present(x_end)) then
      xe = x_end
    else
      xe = x(size(x))
    end if

    ! Apply smoothing to entire array if requested

    allocate(y_work(size(y)))

    do_smooth = .false.
    if (present(smooth)) do_smooth = smooth

    if (do_smooth) then
      ! Window size: ~3% of total points, minimum 5
      n_window = max(5, size(y) / 30)
      n_half = n_window / 2

      ! Moving average
      do i = 1, size(y)
        y_work(i) = sum(y(max(1, i - n_half) : min(size(y), i + n_half))) / &
                    dble(min(size(y), i + n_half) - max(1, i - n_half) + 1)
      end do
    else
      ! No smoothing - use original y
      y_work = y
    end if

    ! Single pass through arrays counting reversals
    n = 0
    y_prev = 0.0d0
    
    do i = 1, size(y)
      if (abs(y_work(i)) >= threshold) then
        in_range = (x(i) >= xs .and. x(i) <= xe)
        
        if (y_prev /= 0.0d0 .and. in_range) then
          if (y_prev * y_work(i) < 0.0d0) then
            n = n + 1
          end if
        end if
        
        y_prev = y_work(i)
      end if
    end do

    deallocate(y_work)

  end function



  function derivative1(x, y)

    !------------------------------------------------------------------------------
    !! get first derivative of polyline (x,y)
    !     using Backward, center, forward adapted difference approximation 
    !------------------------------------------------------------------------------

    double precision, dimension(:), intent(in) :: x, y
    double precision, dimension(size(x)) :: derivative1
    double precision, dimension(:), allocatable :: h, h_minus, hr
    integer :: npt

    npt = size(x)
  
    if (npt < 2) return

    ! Forward difference for first point
    derivative1(1) = (y(2) - y(1)) / (x(2) - x(1))
    
    if (npt == 2) return

    ! Central difference for interior points (vectorized)
    allocate(h(npt-2), h_minus(npt-2), hr(npt-2))
    h       = x(3:npt) - x(2:npt-1)
    h_minus = x(2:npt-1) - x(1:npt-2)
    hr      = h / h_minus
    derivative1(2:npt-1) = (y(3:npt) - hr*hr*y(1:npt-2) - (1.d0 - hr*hr)*y(2:npt-1)) / &
                           (h * (1.d0 + hr))
    deallocate(h, h_minus, hr)

    ! Backward difference for last point
    derivative1(npt) = (y(npt) - y(npt-1)) / (x(npt) - x(npt-1))

  end function 


  function round_scalar(x, decimals) result(r)

    !! rounds x to 'decimals' decimal places
    !! return value is never -0.0

    double precision, intent(in)  :: x
    integer, intent(in)           :: decimals
    double precision              :: r, factor

    factor = 10d0 ** decimals
    r = anint(x * factor) / factor
    if (r == 0d0) r = 0d0          ! eliminate -0.0

  end function round_scalar


  function round_array(x, decimals) result(r)

    !! rounds array x(:) to 'decimals' decimal places
    !! return values are never -0.0

    double precision, dimension(:), intent(in) :: x
    integer, intent(in)                        :: decimals
    double precision, dimension(size(x))       :: r
    double precision                           :: factor

    factor = 10d0 ** decimals
    r = anint(x * factor) / factor
    where (r == 0d0) r = 0d0          ! eliminate -0.0

  end function round_array


  function binary_search_u(f, target_x, low, high, max_iter) result(u)
    !---------------------------------------------------------------------------
    !! Binary search for parameter u in [low,high] such that f(u) ≈ target_x.
    !! f must be monotonically increasing. Each step halves the bracket;
    !! 5 steps (default) → bracket ≤ (high-low)/32 ≈ 0.03 for a unit interval.
    !---------------------------------------------------------------------------
    interface
      function f(t) result(x)
        double precision, intent(in) :: t
        double precision             :: x
      end function f
    end interface
    double precision, intent(in)           :: target_x, low, high
    integer,          intent(in), optional :: max_iter
    double precision                       :: u

    double precision :: lo, hi, mid
    integer          :: i, niter

    niter = 5
    if (present(max_iter)) niter = max_iter

    lo = low
    hi = high
    do i = 1, niter
      mid = 0.5d0 * (lo + hi)
      if (f(mid) < target_x) then
        lo = mid
      else
        hi = mid
      end if
    end do
    u = 0.5d0 * (lo + hi)

  end function binary_search_u


end module
