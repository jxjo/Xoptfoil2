! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2025 Jochen Guenzel 

module simplex_search

  use os_util
  use print_util

! Simplex search (Nelder-Mead) optimization routine

  implicit none
  private


  type simplex_options_type
  double precision  :: min_radius             ! tolerance in simplex radius before triggering a stop
  double precision  :: initial_step = 0.02d0  ! inital step size in normed space 0..1
  integer           :: max_iterations         ! Max steps allowed before stopping
  end type simplex_options_type

  public :: simplex_options_type
  public :: simplexsearch

contains

  subroutine simplexsearch (xopt, fmin, steps, fevals, objfunc, x0_in, given_f0_ref,  &
                            f0_ref, sx_options)

    !----------------------------------------------------------------------------
    !
    !! Nelder-Mead simplex search algorithm
    !
    !! xopt          out: designvars result 
    !! fmin          out: smallest value of objective function     
    !! steps         out: iteration steps needed
    !! fevals        out: number of evaluation of objective function 
    !! objfunc       interface objective function 
    !! x0            start values of designvars 
    !! given_f0_ref  is there a reference reference start value of objective function 
    !! f0_ref        inout: reference start value of objective function 
    !----------------------------------------------------------------------------

    double precision, dimension(:), intent(inout) :: xopt
    double precision, intent(out) :: fmin
    integer, intent(out) :: steps, fevals

    interface
      double precision function objfunc(v)
        double precision, intent(in) :: v (:)
      end function
    end interface

    double precision, dimension(:), intent(in) :: x0_in
    double precision, intent(inout) :: f0_ref
    logical, intent(in) :: given_f0_ref
    type (simplex_options_type), intent(in) :: sx_options

    double precision, dimension(size(x0_in),size(x0_in,1)+1) :: dv
    double precision, dimension(size(x0_in)+1) :: objvals
    double precision, dimension(size(x0_in)) :: xcen, xr, xe, xc, x0 

    double precision :: rho, xi, gam, sigma, fr, fe, fc, f0, mincurr, radius, step
    integer :: i, j, nvars, designcounter
    logical :: converged, needshrink, signal_progress

    ! Standard Nelder-Mead constants

    rho = 1.d0
    xi = 2.d0
    gam = 0.5d0
    sigma = 0.5d0

    step  = sx_options%initial_step                  ! look-around radius in initial step


    ! Sanity check - xo inside normed search space 0..1
    x0 = x0_in 

    do i = 1, size(x0)
      if (x0(i) < 0d0 .or. x0(i) > 1.d0) then 
        call print_error ("Simplex: Initial x0("//stri(i)//") outside bounds") 
        x0(i) = 0.5d0                                 ! set in the middle 
      end if 

    end do 

    ! Set up or read initialzation data

    nvars = size(x0,1)


    ! Get f0 (reference seed design objective function)

    if (given_f0_ref) then
      f0 = f0_ref
    else 
      f0 = objfunc(x0)
      f0_ref = f0
    end if

    ! Set up initial simplex

    fevals = 0
    do j = 1, nvars
      do i = 1, nvars
        if (i == j) then
          if (x0(i) == 0.d0) then
            dv(i,j) = step
          else
            if ((x0(i) + step) > 1d0) then 
              dv(i,j) = x0(i) - step  
            else 
              dv(i,j) = x0(i) + step  
            end if 
          end if
        else
          dv(i,j) = x0(i)
        end if
      end do

      if (maxval(dv(:,j)) > 1d0 .or. minval(dv(:,j)) < 0d0) then    ! check boundaries
        objvals(j) = 9999d0
      else
        objvals(j) = objfunc(dv(:,j))
      end if 
      fevals = fevals + 1
    end do

    dv(:,nvars+1) = x0

    if (maxval(x0) > 1d0 .or. minval(x0) < 0d0) then    ! check boundaries 
      objvals(nvars+1) = 9999d0
    else
      objvals(nvars+1) = objfunc(x0)
    end if 

    fevals = fevals + 1

    ! Counters

    steps = 0
    designcounter = 0

    ! Initial minimum value

    fmin = minval(objvals)
    mincurr = fmin

    ! Iterative procedure for optimization
  
    needshrink = .false.
    converged = .false.

    main_loop: do while (.not. converged)

      steps = steps + 1
      if (steps == sx_options%max_iterations) converged = .true.
      
      ! Sort according to ascending objective function value

      call bubble_sort(dv, objvals)
      mincurr = objvals(1)

      ! Update fmin if appropriate

      if (mincurr < fmin) then
        fmin = mincurr
        signal_progress = .true.
      else
        signal_progress = .false.
      end if

      ! Check for convergence

      radius = design_radius(dv)
      if (radius < sx_options%min_radius) converged = .true.

      ! Compute the centroid of the best nvals designs

      xcen(:) = 0.d0
      do i = 1, nvars
        xcen = xcen + dv(:,i)
      end do
      xcen = xcen/dble(nvars)

      ! Compute the reflection point and evaluate its objective function value

      xr = (1.d0 + rho)*xcen - rho*dv(:,nvars+1)

      if (maxval(xr) > 1d0 .or. minval(xr) < 0d0) then    ! check boundaries 
        fr = 9999d0
      else
        fr = objfunc(xr)
      end if 
      fevals = fevals + 1

      expand_or_contract: if (objvals(1) <= fr .and. fr < objvals(nvars)) then

        ! Accept reflection point

        dv(:,nvars+1) = xr
        objvals(nvars+1) = fr
        cycle

      elseif (fr < objvals(1)) then

        ! Expand

        xe = (1.d0 + rho*xi)*xcen - rho*xi*dv(:,nvars+1)

        if (maxval(xe) > 1d0 .or. minval(xe) < 0d0) then    ! check boundaries 
          fe = 9999d0
        else
          fe = objfunc(xe)
        end if 
    
        fevals = fevals + 1
        if (fe < fr) then
          dv(:,nvars+1) = xe
          objvals(nvars+1) = fe
        else
          dv(:,nvars+1) = xr
          objvals(nvars+1) = fr
        end if
        cycle

      elseif (fr >= objvals(nvars)) then

          ! Outside contraction

          contraction: if (fr < objvals(nvars+1)) then

            xc = (1.d0 + rho*gam)*xcen - rho*gam*dv(:,nvars+1)
            if (maxval(xc) > 1d0 .or. minval(xc) < 0d0) then    ! check boundaries 
              fc = 9999d0
            else
              fc = objfunc(xc)
            end if 
            fevals = fevals + 1

            if (fc < fr) then
              dv(:,nvars+1) = xc
              objvals(nvars+1) = fc
              needshrink = .false.
            else
              needshrink = .true.
            end if

          ! Inside contraction

          else 

            xc = (1.d0 - gam)*xcen + gam*dv(:,nvars+1)
            if (maxval(xc) > 1d0 .or. minval(xc) < 0d0) then    ! check boundaries 
              fr = 9999d0
            else
              fc = objfunc(xc)
            end if 
            fevals = fevals + 1
            
            if (fc < objvals(nvars+1) ) then
              dv(:,nvars+1) = xc
              objvals(nvars+1) = fc
              needshrink = .false.
            else
              needshrink = .true.
            end if

          end if contraction

          ! Shrink

          shrink: if (needshrink) then

            do i = 2, nvars + 1
              dv(:,i) = dv(:,1) + sigma*(dv(:,i) - dv(:,1))
              if (maxval(dv(:,i)) > 1d0 .or. minval(dv(:,i)) < 0d0) then    ! check boundaries 
                objvals(i) = 9999d0
              else
                objvals(i) = objfunc(dv(:,i))
              end if 
              fevals = fevals + 1
            end do
            cycle

          else

            cycle

          end if shrink

      end if expand_or_contract

    end do main_loop

    ! Sort one more time according to ascending objective function value

    call bubble_sort(dv, objvals)
    xopt = dv(:,1)
    fmin = objvals(1)

    ! Check for convergence one more time

    radius = design_radius(dv)

  end subroutine simplexsearch



  subroutine bubble_sort(dv, objvals)

    !----------------------------------------------------------------------------
    !! Sorts a set of designs according to their objective function value
    !----------------------------------------------------------------------------
  
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
  
  


  function design_radius(dv)

    !----------------------------------------------------------------------------
    !! Computes max radius of designs (used for evaluating convergence)
    !----------------------------------------------------------------------------
  
    use math_util, only : norm_2
  
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
  
  
end module simplex_search
