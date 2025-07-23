! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2020-2025 Jochen Guenzel

module math_util

  ! Various basic math functions and numerical methods

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



  function norm_2(vector) result (val)

    !----------------------------------------------------------------------------
    !! Vector norm (since not all compilers may include it by default)
    !----------------------------------------------------------------------------

    double precision, dimension(:), intent(in) :: vector
    double precision :: val
    integer :: i

    val = 0.d0
    do i = 1, size(vector)
      val = val + vector(i)**2.d0
    end do
    val = sqrt(val)

  end function 



  function norm2p (x,y) result (val)

    !----------------------------------------------------------------------------
    !! norm2 of two coordinates (for convenience) 
    !----------------------------------------------------------------------------

    double precision, intent(in) :: x, y
    double precision :: val

    val = sqrt(x**2 + y**2)

  end function 



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



  subroutine find_reversals(istart, iend, threshold, y, reversal_sign, &
                            nreversals, result_info)

    !----------------------------------------------------------------------------
    !! Finds and counts reversals of a of polyline (x,y) - only the y-values are needed
    !    
    ! To find the "real" reversals only y value greater threshold
    !    are taken to check against the change from + to -
    !
    ! istart and iend define the range of polyline to be scanned.
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

  end subroutine 


  function count_reversals (istart, iend, y, threshold)

    !------------------------------------------------------------------------------
    !! Gets the number of reversals for 'threshold' 
    !------------------------------------------------------------------------------

    integer, intent(in)                         :: istart, iend
    double precision, dimension(:), intent(in)  :: y
    double precision, intent(in)                :: threshold
    integer          :: count_reversals

    call find_reversals(istart, iend, threshold, y, 'R', count_reversals)

  end function 



  function min_threshold_for_reversals (istart, iend, y, min_threshold, max_threshold, &
                                        nreversals_to_detect)

    !------------------------------------------------------------------------------
    !! Evaluates the min possible threshold that no more than nreversals will be
    !!    detected on the polyline (x,y) - only y is needed
    !------------------------------------------------------------------------------

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

  end function 



  function derivative1(x, y)

    !------------------------------------------------------------------------------
    !! get first derivative of polyline (x,y)
    !     using Backward, center, forward adapted difference approximation 
    !------------------------------------------------------------------------------

    double precision, dimension(:), intent(in) :: x, y
    double precision, dimension(size(x)) :: derivative1
    integer :: i
    double precision :: h_minus, h, hr
    integer :: npt

    npt = size(x)
  
    do i = 1, npt
      if (i == 1) then                                                 ! forward
        derivative1(i) = (y(i+1) - y(i))/ (x(i+1) - x(i)) 
      else if (i ==npt) then                                           ! backward
        derivative1(i) = (y(i) - y(i-1))/ (x(i) - x(i-1))
      else                                                             ! center
        h       = x(i+1) - x(i)
        h_minus = x(i) - x(i-1)
        hr      = h / h_minus 
        derivative1(i) = (y(i+1) - hr*hr*y(i-1) -(1-hr*hr)*y(i))/ (h * (1.d0 +hr)) 
      end if 
    end do

  end function 


end module
