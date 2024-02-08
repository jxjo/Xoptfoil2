!  This file is part of XOPTFOIL.

!  XOPTFOIL is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation, either version 3 of the License, or
!  (at your option) any later version.

!  XOPTFOIL is distributed in the hope that it will be useful,
!  but WITHOUT ANY WARRANTY; without even the implied warranty of
!  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!  GNU General Public License for more details.

!  You should have received a copy of the GNU General Public License
!  along with XOPTFOIL.  If not, see <http://www.gnu.org/licenses/>.

!  Copyright (C) 2017-2019 Daniel Prosser

module xfoil_driver

! Contains subroutines to use XFoil to analyze an airfoil

  use os_util 
  use print_util
  use airfoil_operations,    only : airfoil_type


  implicit none

  type re_type 
    double precision :: number            ! Reynolds Number
    integer          :: type              ! Type 1 or 2 (fixed lift)
  end type re_type

  ! Hold result of xfoil boundary layer (BL) infos of an op_pooint

  type bubble_type      
    logical          :: found             ! a bubble was detected           
    double precision :: xstart            ! start of separation: CF (shear stress) < 0
    double precision :: xend              ! end   of separation: CF (shear stress) > 0
  end type bubble_type                              

  ! defines an op_point for xfoil calculation
 
  type op_point_spec_type  
  
    ! aero - xfoil 
    logical                     :: spec_cl                 ! op based on alpha or cl
    double precision            :: value                   ! base value of cl or alpha
    type (re_type)              :: re, ma                  ! Reynolds and Mach 
    double precision            :: ncrit                   ! xfoil ncrit

    ! flap setting
    double precision            :: flap_angle              ! fix flap angle of this op op point 

    ! objective function 
    double precision            :: scale_factor            ! scale for objective function
    character(:), allocatable   :: optimization_type       ! eg 'min-drag'
    double precision            :: target_value            ! target value to achieve
    logical                     :: allow_improved_target   ! the result value may be better than tagrte value
    double precision            :: weighting               ! weighting within objective function
    double precision            :: weighting_user          ! original weighting entered by user

    !dynamic weighting 
    logical                     :: dynamic_weighting       ! dynamic weighting for this point 
    logical                     :: extra_punch             !  - this op got an extra weighting punch
    double precision            :: weighting_user_cur      !  - info: current scaled user weighting
    double precision            :: weighting_user_prv      !  - info: previous scaled user weighting

  end type op_point_spec_type


  ! Specify flap

  type flap_spec_type
    logical          :: use_flap                  ! activate flaps
    integer          :: ndv                       ! no of design variables = no of flaps to optimize
    double precision :: x_flap, y_flap 
    character(3)     :: y_flap_spec
    double precision :: min_flap_degrees
    double precision :: max_flap_degrees
  end type flap_spec_type


  ! Hold result of xfoil aero calculation of an op_pooint

  type op_point_result_type                              
    logical                     :: converged                ! did xfoil converge? 
    double precision            :: cl                       ! lift coef.  - see also spec_cl
    double precision            :: alpha                    ! alpha (aoa) - see also spec_cl
    double precision            :: cd                       ! drag coef.  
    double precision            :: cm                       ! moment coef. 
    double precision            :: xtrt                     ! point of transition - top side 
    double precision            :: xtrb                     ! point of transition - bottom side 
    type (bubble_type) :: bubblet, bubbleb! bubble info - top and bottom 

  end type op_point_result_type                              


  ! defines xfoils calculation environment

  type xfoil_options_type
    double precision :: ncrit             ! Critical ampl. ratio
    double precision :: xtript, xtripb    ! forced trip locations
    logical :: viscous_mode               ! do viscous calculation           
    logical :: silent_mode                ! Toggle xfoil screen write
    logical :: show_details               ! show some user entertainment 
    integer :: maxit                      ! max. iterations for BL calcs
    double precision :: vaccel            ! xfoil BL convergence accelerator
    logical :: fix_unconverged            ! try to fix unconverged pts.
    logical :: detect_outlier             ! try to detect op point outlier during optimization
    logical :: exit_if_unconverged        ! exit die op point loop if a point is unconverged
    logical :: reinitialize               ! reinitialize BLs per op_point
  end type xfoil_options_type


  ! result statistics for drag(lift) outlier detetction 

  type value_statistics_type   
    logical :: no_check                ! deactivate detection e.g. when flaps are set
    integer :: nvalue                  ! total numer of values tested
    double precision :: minval         ! the smallest value up to now 
    double precision :: maxval         ! the biggest value up to now 
    double precision :: meanval        ! the average value up to now 

  end type value_statistics_type


  ! --- static, private ------------------------------------------

  type (value_statistics_type), dimension(:), allocatable, private :: drag_stats

  contains


!=============================================================================
!
! Core routine to run xfoil calculation for each operating points
!      returns Cl, Cd, Cm, ... for an airfoil
!
!=============================================================================

subroutine run_op_points (foil, xfoil_options,         &
                          flap_spec, flap_degrees, &
                          op_points_spec, op_points_result)

  use xfoil_inc    

  type(airfoil_type), intent(in)            :: foil
  type(xfoil_options_type),      intent(in) :: xfoil_options
  type(flap_spec_type),          intent(in) :: flap_spec
  type(op_point_spec_type), dimension(:), intent(in)  :: op_points_spec
  type(op_point_result_type), dimension(:), allocatable, intent(out) :: op_points_result
  double precision, dimension(:), intent(in) :: flap_degrees


  integer :: i, noppoint
  integer :: iretry, nretry
  double precision :: prev_op_delta, op_delta, prev_flap_degree, prev_op_spec_value
  logical:: point_fixed, show_details, flap_changed, prev_op_spec_cl, detect_outlier
  type(op_point_spec_type) :: op_spec, tmp_op_spec
  type(op_point_result_type)        :: op, tmp_op

  noppoint = size(op_points_spec,1) 

! Sanity checks

  if (.not. allocated(AIJ)) then
    call my_stop ("xfoil is not initialized.")
  end if
  if (noppoint == 0) then 
    call my_stop ("Error in xfoil_driver: No operating points.")
  end if

! Init variables

  
  allocate (op_points_result(noppoint))

  op_points_result%converged = .false.          ! init - watch "early exit" 
  op_points_result%cl        = 0d0  
  op_points_result%cd        = 0d0  
  op_points_result%alpha     = 0d0  
  op_points_result%cm        = 0d0             
  op_points_result%xtrt      = 0d0               
  op_points_result%xtrb      = 0d0              
  
  prev_op_delta   = 0d0
  prev_op_spec_cl = op_points_spec(1)%spec_cl
  flap_changed = .false.
  prev_flap_degree = 999d0 
!  prev_flap_degree =   flap_degrees (1) 
  show_details   = xfoil_options%show_details
  detect_outlier = xfoil_options%detect_outlier


! init statistics for out lier detection the first time and when polar changes
  if (detect_outlier) then 
!$omp critical
    if (.not. allocated(drag_stats)) then
      call init_statistics (noppoint)
    else if (size(drag_stats) /= noppoint) then 
      call init_statistics (noppoint)
    end if
!$omp end critical
  end if


! Set default Xfoil parameters 
  call xfoil_defaults(xfoil_options)

  ! ! Set paneling options
  ! call xfoil_set_paneling(geom_options)

  if (show_details) then 
    call print_text ('Xfoil op points: ', 5, no_crlf=.true.)
    if (xfoil_options%reinitialize) call print_colored (COLOR_NOTE, 'init_BL ')
  end if


! Run xfoil for requested operating points -----------------------------------
!
! Rules for initialization of xfoil boundary layer - xfoil_init_BL 
!
!   xfoil_options%reinitialize = 
!   .true.    init will be one before *every* xfoil calculation 
!   .false.   init will be done only
!             - at the first op_point
!             - when the flap angle is changed (new foil is set)
!             - when a point didn't converge
!             - when the direction of alpha or cl changes along op points

  do i = 1, noppoint

    op_spec = op_points_spec(i)

!   print newline if output gets too long
    if (show_details .and.( mod(i,80) == 0)) write (*,'(/,7x,A)',advance = 'no') '       '

!   if flpas are activated, check if the angle has changed to reinit foil

    if(flap_spec%use_flap .and. (flap_degrees(i) /= prev_flap_degree)) then 
      flap_changed = .true.
      prev_flap_degree = flap_degrees(i)
      if (allocated(drag_stats)) drag_stats%no_check = .true.  !deactivate outlier detection when flapping
    else
      flap_changed = .false.
    end if 

!   set airfoil, apply flap deflection, init BL if needed
    if (flap_changed .or. (i == 1)) then

      ! set airfoil into xfoil buffer
      call xfoil_set_airfoil(foil)              ! "restore" current airfoil

      ! apply flap only if set to non zero degrees
      if (flap_changed) then
        call xfoil_apply_flap_deflection(flap_spec, flap_degrees(i))
      end if     

      ! In case of flaps (or first op) always init boundary layer 
      call xfoil_init_BL (show_details)

    else
      ! Init BL always if set in parameters 
      if (xfoil_options%reinitialize) then 
        call xfoil_init_BL (.false.)
      else
        if (op_spec%spec_cl .neqv. prev_op_spec_cl) then  ! init if op_mode changed
          call xfoil_init_BL (show_details)
          prev_op_delta = 0d0
        else                                    ! Init BL if the direction of alpha or cl changes 
          op_delta = op_spec%value - prev_op_spec_value
          if ((prev_op_delta * op_delta) < 0d0) then 
            call xfoil_init_BL (show_details)
            prev_op_delta = 0d0
          else
            prev_op_delta = op_delta
          end if 
        end if
      end if
    end if

    prev_op_spec_value = op_spec%value
    prev_op_spec_cl    = op_spec%spec_cl

!   Now finally run xfoil at op_point
    call run_op_point (op_spec, &
                      xfoil_options%viscous_mode, xfoil_options%maxit, show_details, & 
                      op)


!   Handling of unconverged points
    if (op%converged) then
      if (detect_outlier .and. is_out_lier (i, op%cd)) then
        op%converged = .false.
        if (show_details) call print_colored (COLOR_WARNING, 'flip')
      else if (cl_changed (op_spec%spec_cl, op_spec%value, op%cl)) then
        op%converged = .false.
        if (show_details) call print_colored (COLOR_WARNING, 'lift')
        if (show_details) write (*,'(A)',advance = 'no') 'lift'
      end if 
    end if

    if (.not. op%converged .and. xfoil_options%fix_unconverged) then

      if (show_details) write (*,'(A)',advance = 'no') '['

!     Try to initialize BL at intermediate new point (in the direction away from stall)
      tmp_op_spec = op_spec
      if (op_spec%spec_cl) then
        if (op_spec%value > 0d0) then 
           tmp_op_spec%value = op_spec%value - 0.02d0
        else 
          tmp_op_spec%value = op_spec%value + 0.02d0
        end if 
        if (tmp_op_spec%value == 0.d0) tmp_op_spec%value = 0.01d0       !because of Type 2 polar calc
      else
        if (op_spec%value > 0d0) then 
          tmp_op_spec%value = op_spec%value - 0.25d0
        else 
          tmp_op_spec%value = op_spec%value + 0.25d0
        end if 
      end if

      ! init BL for this new point to start for fix with little increased Re
      tmp_op_spec%re%number = tmp_op_spec%re%number * 1.001d0
      call xfoil_init_BL (show_details .and. (.not. xfoil_options%reinitialize))
      call run_op_point  (tmp_op_spec, xfoil_options%viscous_mode, xfoil_options%maxit, &
                          show_details , tmp_op)

!     If this intermediate point converged
!       try to run again at the old operating point decreasing RE a little ...

      point_fixed = .false.

      if (tmp_op%converged) then 

        iretry = 1
        nretry = 2                ! try more

        do while (.not. point_fixed .and. (iretry <= nretry)) 

          ! op_spec%re%number = op_spec%re%number * 0.998d0 ! result will be worse (penalty!)
          op_spec%re%number = op_spec%re%number * 1.002d0 ! result will be better 

          if (xfoil_options%reinitialize) call xfoil_init_BL (.false.)

          call run_op_point (op_spec, xfoil_options%viscous_mode, xfoil_options%maxit, &
                            show_details, op)
                                
          if (.not. op%converged .or. (detect_outlier .and. is_out_lier (i, op%cd))  &
              .or. (cl_changed (op_spec%spec_cl, op_spec%value, op%cl))) then 
            call xfoil_init_BL (show_details .and. (.not. xfoil_options%reinitialize))
          else 
            point_fixed = .true.
          end if 

          iretry = iretry + 1

        end do
        
        if (point_fixed) then 
          ! call print_colored (COLOR_GOOD, 'f'//stri(i))
        else
          ! call print_colored (COLOR_WARNING, 'x'//stri(i))
        end if
      else
        ! call print_colored (COLOR_BAD, 'x'//stri(i))
      end if  

      if(show_details) then 
        write (*,'(A)',advance = 'no') ']'
        if (point_fixed) then 
          call print_colored (COLOR_NOTE, 'fixed ')
        else
          call print_colored (COLOR_ERROR,  'x')
        end if  
      end if 

!     no fix achieved - reinit BL (for the next op) - set converged flag to .false.
      if(.not. point_fixed) then
        if (.not. xfoil_options%reinitialize) call xfoil_init_BL (show_details)
        op%converged = .false.
      end if
    end if

    op_points_result(i) = op

!   early exit if not converged for speed optimization 
    if ((.not. op%converged) .and. xfoil_options%exit_if_unconverged) then 
      exit
    end if 

!   Print warnings about unconverged points
!   Update statistics
    if (op%converged) then 

      if (detect_outlier) call update_statistic (i, op%cd)      ! Update cd outlier statistics

    ! Support Type 1 and 2 re numbers - cl may not be negative  
      if ((op_spec%re%type == 2) .and. (op%cl <= 0d0)) then 
        write(*,'(15x,A,I2,A, F6.2)') "Warning: Negative lift for Re-Type 2 at" // &
        " op",i," - cl:",op%cl
      end if 
    end if 
 
  end do 
 
  if(show_details) write (*,*) 
  
end subroutine run_op_points

  

!===============================================================================
!
! Runs Xfoil at a specified op_point which is either
!  - at an angle of attack
!  - at an lift coefficient
!
! Assumes airfoil geometry, reynolds number, and mach number have already been 
! set in Xfoil.
!
!===============================================================================

subroutine run_op_point (op_point_spec,        &
                         viscous_mode, maxit, show_details,  &
                         op_point_result)

  use xfoil_inc

  type(op_point_spec_type), intent(in)  :: op_point_spec
  logical,                           intent(in)  :: viscous_mode, show_details
  integer,                           intent(in)  :: maxit
  type(op_point_result_type),        intent(out) :: op_point_result

  integer         :: niter_needed
  doubleprecision :: save_ACRIT


  op_point_result%cl    = 0.d0
  op_point_result%cd    = 0.d0
  op_point_result%alpha = 0.d0
  op_point_result%cm    = 0.d0
  op_point_result%xtrt  = 0.d0
  op_point_result%xtrb  = 0.d0
  op_point_result%converged = .true.

! Support Type 1 and 2 re numbers  
  REINF1 = op_point_spec%re%number
  RETYP  = op_point_spec%re%type 
  MATYP  = op_point_spec%ma%type 
  call MINFSET(op_point_spec%ma%number)

! Set compressibility parameters from MINF
  CALL COMSET

! Set ncrit per point
  save_ACRIT = ACRIT
  if (op_point_spec%ncrit /= -1d0) ACRIT = op_point_spec%ncrit

! Inviscid calculations for specified cl or alpha
  if (op_point_spec%spec_cl) then
    LALFA = .FALSE.
    ALFA = 0.d0
    CLSPEC = op_point_spec%value
    call SPECCL
  else 
    LALFA = .TRUE.
    ALFA = op_point_spec%value * DTOR
    call SPECAL
  end if

  if (abs(ALFA-AWAKE) .GT. 1.0E-5) LWAKE  = .false.
  if (abs(ALFA-AVISC) .GT. 1.0E-5) LVCONV = .false.
  if (abs(MINF-MVISC) .GT. 1.0E-5) LVCONV = .false.

  ! Viscous calculations (if requested)

  op_point_result%converged = .true. 

  if (viscous_mode) then 
    
    call VISCAL(maxit, niter_needed)

    ! coverged? 

    if (niter_needed > maxit) then 
      op_point_result%converged = .false.
    ! RMSBL equals to viscrms() formerly used...
    else if (.not. LVCONV .or. (RMSBL > 1.D-4)) then 
      op_point_result%converged = .false.
    else 
      op_point_result%converged = .true.
    end if

  end if

  ! Restore default ncrit 

  ACRIT = save_ACRIT
 
  ! Outputs

  op_point_result%cl    = CL
  op_point_result%cm    = CM
  op_point_result%alpha = ALFA/DTOR

  if (viscous_mode) then 
    op_point_result%cd   = CD 
    op_point_result %xtrt = XOCTR(1)
    op_point_result%xtrb = XOCTR(2)
    if (op_point_result%converged) then 
      ! call detect_bubble (op_point_result%bubblet, op_point_result%bubbleb)
      op_point_result%bubblet%found = .false.
      op_point_result%bubbleb%found = .false.
    end if
  else
    op_point_result%cd   = CDP
    op_point_result%xtrt = 0.d0
    op_point_result%xtrb = 0.d0
    op_point_result%bubblet%found = .false.
    op_point_result%bubbleb%found = .false.
  end if

! Final check for NaNs

  if (isnan(op_point_result%cl)) then
    op_point_result%cl = -1.D+08
    op_point_result%converged = .false.
  end if
  if (isnan(op_point_result%cd)) then
    op_point_result%cd = 1.D+08
    op_point_result%converged = .false.
  end if
  if (isnan(op_point_result%cm)) then
    op_point_result%cm = -1.D+08
    op_point_result%converged = .false.
  end if

  if(show_details) then 
    if (op_point_result%converged) then
      !call print_colored (COLOR_NORMAL,  '.' // stri(niter_needed))
      call print_colored (COLOR_NORMAL,  '.')
    else
      !call print_colored (COLOR_WARNING, 'x' // stri(niter_needed))
      call print_colored (COLOR_WARNING, 'x')
    end if
  end if

! when xfoil doesn't converge, it tends to write invalid values in the original field ...
  if (.not. op_point_result%converged) then
    if (op_point_spec%spec_cl) then
      op_point_result%cl    = op_point_spec%value
    else 
      op_point_result%alpha = op_point_spec%value 
    end if
  end if 


end subroutine run_op_point


!-------------------------------------------------------------------------------
! Detect a bubble on top or bottom side using xfoil TAU (shear stress) info
!    
!       If TAU < 0 and end of laminar separation is before transition point 
!       it should be a bubble
!
! with re-attachment If TAU < 0 and end of laminar separation is after transition point
!              xstart ooooooooooooooooo xend
!    -----------------------~~~~~~~~~~~~~~~~~~~~~~~~~~
!                          xtr
!
! without re-attachment - If TAU < 0 and end of laminar separation is before transition point 
!              xstart ooooooooooo xend
!    -------------------------------------~~~~~~~~~~~~
!                                        xtr
!
! Code is inspired from Enno Eyb / xoper.f from xfoil source code
!-------------------------------------------------------------------------------

subroutine detect_bubble (bubblet, bubbleb)

  use xfoil_inc

  type (bubble_type), intent(inout) :: bubblet, bubbleb
  double precision :: CF 
  double precision :: detect_xstart, detect_xend 
  integer :: I, IS, IBL
    
  bubblet%found  = .false.
  bubblet%xstart = 0d0
  bubblet%xend   = 0d0

  bubbleb%found  = .false.
  bubbleb%xstart = 0d0
  bubbleb%xend   = 0d0

  detect_xstart = 0.05d0              ! detection range 
  detect_xend   = 1d0
  

  !!Write CF to file together with results
  !open  (unit=iunit, file='CF.txt')
  ! write(*,'(A)') '    X       Y       CF'

! Detect range on upper/lower side where shear stress < 0 

! --- This is the Original stripped down from XFOIL 
  do I=1, N

    if((X(I) >= detect_xstart) .and. (X(I) <= detect_xend)) then

      IS = 1
      IF(GAM(I) .LT. 0.0) IS = 2
    
      IF(LIPAN .AND. LVISC) THEN        ! bl calc done and viscous mode? 
        IF(IS.EQ.1) THEN
          IBL = IBLTE(IS) - I + 1
        ELSE
          IBL = IBLTE(IS) + I - N
        ENDIF
        CF =  TAU(IBL,IS)/(0.5*QINF**2)
      ELSE
        CF = 0.
      ENDIF
  ! --- End Original stripped down from XFOIL 

      if (IS == 1) then                 ! top side - going from TE to LE 
        ! if ((X(I) <= XOCTR(1)) .and. (.not.bubblet%found) )  then  ! no bubbles after transition point
        if (.not.bubblet%found)  then   
          if     ((CF < 0d0) .and. (bubblet%xend == 0d0)) then
            bubblet%xend = X(I) 
          elseif ((CF < 0d0) .and. (bubblet%xend > 0d0)) then 
            bubblet%xstart   = X(I) 
          elseif ((CF >= 0d0) .and. (bubblet%xstart > 0d0)) then 
            bubblet%xstart   = X(I-1) 
            bubblet%found = .true.
          end if 
        end if 
        !if ((bubblet%xstart > 0d0) .and. (bubblet%xend > 0d0)) then 
        ! print *, ' --- top ------', i, x(i), CF, XOCTR(1), bubblet%found
        !end if 

      else                                    ! bottom side - going from LE to TE 
        if( .not.bubbleb%found )  then       
          if     ((CF < 0d0) .and. (bubbleb%xstart == 0d0)) then
            bubbleb%xstart = X(I) 
          elseif ((CF < 0d0) .and. (bubbleb%xstart > 0d0)) then 
            bubbleb%xend   = X(I) 
          elseif ((CF >= 0d0) .and. (bubbleb%xend > 0d0)) then 
            bubbleb%xend   = X(I-1) 
            bubbleb%found = .true.
            !print *, ' --- bot ------', i, x(i), CF, XOCTR(2), bubbleb%found
          end if 
        end if  
      end if  
    
    end if 

  end do

  ! bubble end was not detected - take beginning of transition for end
  if ((bubbleb%xstart > 0d0)  .and. (bubbleb%xend == 0d0)) then 
    bubbleb%xend = X(N)
    bubbleb%found  = .true.
    ! print '(" ---- bubble bot no end! start end ", 3F6.3)', bubbleb%xstart, bubbleb%xend, XOCTR(2)
  end if
 
  ! a real, good bubble should be longer than 0.01

  if (bubblet%found .and. ((bubblet%xend - bubblet%xstart) < 0.01d0) ) bubblet%found  = .false.
  if (bubbleb%found .and. ((bubbleb%xend - bubbleb%xstart) < 0.01d0) ) bubbleb%found  = .false.
  
  if (.not. bubblet%found) then
    bubblet%xstart = 0d0
    bubblet%xend = 0d0
  end if 
  if (.not. bubbleb%found) then
    bubbleb%xstart = 0d0
    bubbleb%xend = 0d0
  end if 

  return

end subroutine detect_bubble
  


! subroutine xfoil_repanel (foilin, foilout, geom_options)

!   !-----------------------------------------------------------------------------
!   !! Repanel a foil using Xfoil's PANGEN subroutine
!   !-----------------------------------------------------------------------------

!   use xfoil_inc

!   type(airfoil_type), intent(in)  :: foilin
!   type(airfoil_type), intent(out) :: foilout
!   type(xfoil_geom_options_type), intent(in) :: geom_options

!   integer :: i
!   logical :: needs_cleanup

!   ! Some things that need to be allocated for XFoil PANGEN

!   needs_cleanup = .false.
!   if (.not. allocated(W1)) then
!     allocate(W1(6*IQX))
!     allocate(W2(6*IQX))
!     allocate(W3(6*IQX))
!     allocate(W4(6*IQX))
!     allocate(W5(6*IQX))
!     allocate(W6(6*IQX))
!     needs_cleanup = .true.
!   end if

!   ! Set some things that Xfoil may need to do paneling

!   PI = 4.d0*atan(1.d0)
!   HOPI = 0.5d0/PI
!   QOPI = 0.25d0/PI
!   SIG(:) = 0.d0
!   NW = 0
!   AWAKE = 0.d0
!   LWDIJ = .false.
!   LIPAN = .false.
!   LBLINI = .false.
!   WAKLEN = 1.d0
!   GAM(:) = 0.d0
!   SIGTE = 0.d0
!   GAMTE = 0.d0
!   SIGTE_A = 0.d0
!   GAMTE_A = 0.d0
!   SILENT_MODE = .TRUE.

!   ! Set xfoil airfoil and paneling options

!   call xfoil_set_airfoil(foilin)
!   call xfoil_set_paneling(geom_options)

!   ! Smooth paneling with PANGEN

!   call PANGEN(.NOT. SILENT_MODE)

!   ! Put smoothed airfoil coordinates into derived type

!   foilout%npoint = geom_options%npan
!   allocate(foilout%x(foilout%npoint))
!   allocate(foilout%y(foilout%npoint))
!   do i = 1, foilout%npoint
!     foilout%x(i) = X(i)
!     foilout%y(i) = Y(i)
!   end do

!   foilout%name = foilin%name
  
!   ! Deallocate memory that is not needed anymore

!   if (needs_cleanup) then
!     deallocate(W1)
!     deallocate(W2)
!     deallocate(W3)
!     deallocate(W4)
!     deallocate(W5)
!     deallocate(W6)
!   end if
  
! end subroutine xfoil_repanel




!=============================================================================80
!
! Subroutine to apply a flap deflection to the buffer airfoil and set it as the
! current airfoil.  For best results, this should be called after PANGEN.
!
!=============================================================================80
subroutine xfoil_apply_flap_deflection(flap_spec, degrees)

  use xfoil_inc
 
  type(flap_spec_type),   intent(in) :: flap_spec

  double precision, intent(in) :: degrees
  
  integer y_flap_spec_int

  if (flap_spec%y_flap_spec == 'y/c') then
    y_flap_spec_int = 0
  else
    y_flap_spec_int = 1
  end if

! Apply flap deflection

  ! caution: FLAP will change y_flap a little --> ()
  call FLAP((flap_spec%x_flap), (flap_spec%y_flap), y_flap_spec_int, degrees)

end subroutine xfoil_apply_flap_deflection


!=============================================================================80
!
! Allocates xfoil variables that may be too big for the stack in OpenMP
!
!=============================================================================80
subroutine xfoil_init()

  use xfoil_inc

! Allocate variables that may be too big for the stack in OpenMP

  allocate(AIJ(IQX,IQX))
  allocate(BIJ(IQX,IZX))
  allocate(DIJ(IZX,IZX))
  allocate(CIJ(IWX,IQX))
  allocate(IPAN(IVX,ISX))
  allocate(ISYS(IVX,ISX))
  allocate(W1(6*IQX))
  allocate(W2(6*IQX))
  allocate(W3(6*IQX))
  allocate(W4(6*IQX))
  allocate(W5(6*IQX))
  allocate(W6(6*IQX))
  allocate(VTI(IVX,ISX))
  allocate(XSSI(IVX,ISX))
  allocate(UINV(IVX,ISX))
  allocate(UINV_A(IVX,ISX))
  allocate(UEDG(IVX,ISX))
  allocate(THET(IVX,ISX))
  allocate(DSTR(IVX,ISX))
  allocate(CTAU(IVX,ISX))
  allocate(MASS(IVX,ISX))
  allocate(TAU(IVX,ISX))
  allocate(DIS(IVX,ISX))
  allocate(CTQ(IVX,ISX))
  allocate(DELT(IVX,ISX))
  allocate(TSTR(IVX,ISX))
  allocate(USLP(IVX,ISX))
  allocate(VM(3,IZX,IZX))
  allocate(VA(3,2,IZX))
  allocate(VB(3,2,IZX))
  allocate(VDEL(3,2,IZX))

end subroutine xfoil_init

!=============================================================================80
!
! Initializes xfoil variables
!
!=============================================================================80
subroutine xfoil_defaults(xfoil_options)

  use xfoil_inc

  type(xfoil_options_type), intent(in) :: xfoil_options

  N = 0
  SILENT_MODE = xfoil_options%silent_mode
  PI = 4.d0*atan(1.d0)
  HOPI = 0.5d0/PI
  QOPI = 0.25d0/PI
  DTOR = PI/180.d0
  QINF = 1.d0
  SIG(:) = 0.d0
  QF0(:) = 0.d0
  QF1(:) = 0.d0
  QF2(:) = 0.d0
  QF3(:) = 0.d0
  NW = 0
  RETYP = 1
  MATYP = 1
  GAMMA = 1.4d0
  GAMM1 = GAMMA - 1.d0
  XCMREF = 0.25d0
  YCMREF = 0.d0
  LVISC = xfoil_options%viscous_mode
  AWAKE = 0.d0
  AVISC = 0.d0
  ITMAX = xfoil_options%maxit
  LWDIJ = .false.
  LIPAN = .false.
  LBLINI = .false.
  ACRIT = xfoil_options%ncrit
  IDAMP = 0
  XSTRIP(1) = xfoil_options%xtript
  XSTRIP(2) = xfoil_options%xtripb
  VACCEL = xfoil_options%vaccel
  WAKLEN = 1.d0
  PSIO = 0.d0
  GAMU(:,:) = 0.d0
  GAM(:) = 0.d0
  SIGTE = 0.d0
  GAMTE = 0.d0
  SIGTE_A = 0.d0
  GAMTE_A = 0.d0
  APANEL(:) = 0.d0

! Set boundary layer calibration parameters

  call BLPINI

end subroutine xfoil_defaults

!=============================================================================80
!
! Sets airfoil for xfoil into buffer and current airfoil
!
!=============================================================================80
subroutine xfoil_set_airfoil(foil)

  use xfoil_inc, only : XB, YB, NB, SB, XBP, YBP 
  type(airfoil_type), intent(in) :: foil

! Set foil into xfoil buffer foil
  NB = size(foil%x)
  XB(1:NB) = foil%x
  YB(1:NB) = foil%y

  CALL SCALC(XB,YB,SB,NB)
  CALL SEGSPL(XB,XBP,SB,NB)
  CALL SEGSPL(YB,YBP,SB,NB)

! Also copy buffer airfoil to xfoil current foil. This is also made in PANGEN -
!        ... but PANGEN shouldn't always be called before xfoil calculations
  call ABCOPY (.true.)

end subroutine xfoil_set_airfoil


  ! subroutine xfoil_set_paneling(geom_options)

  !   use xfoil_inc, only : NPAN, CVPAR, CTERAT, CTRRAT, XSREF1, XSREF2, XPREF1,   &
  !                         XPREF2

  !   type(xfoil_geom_options_type), intent(in) :: geom_options
  !   NPAN = geom_options%npan
  !   CVPAR = geom_options%cvpar
  !   CTERAT = geom_options%cterat
  !   CTRRAT = geom_options%ctrrat
  !   XSREF1 = geom_options%xsref1
  !   XSREF2 = geom_options%xsref2
  !   XPREF1 = geom_options%xpref1
  !   XPREF2 = geom_options%xpref2
    
  ! end subroutine xfoil_set_paneling

!=============================================================================80
!
! Deallocates memory in xfoil
!
!=============================================================================80
subroutine xfoil_cleanup()

  use xfoil_inc

! Deallocate variables
  if (allocated (AIJ)) then
    deallocate(AIJ)
    deallocate(BIJ)
    deallocate(DIJ)
    deallocate(CIJ)
    deallocate(IPAN)
    deallocate(ISYS)
    deallocate(W1)
    deallocate(W2)
    deallocate(W3)
    deallocate(W4)
    deallocate(W5)
    deallocate(W6)
    deallocate(VTI)
    deallocate(XSSI)
    deallocate(UINV)
    deallocate(UINV_A)
    deallocate(UEDG)
    deallocate(THET)
    deallocate(DSTR)
    deallocate(CTAU)
    deallocate(MASS)
    deallocate(TAU)
    deallocate(DIS)
    deallocate(CTQ)
    deallocate(DELT)
    deallocate(TSTR)
    deallocate(USLP)
    deallocate(VM)
    deallocate(VA)
    deallocate(VB)
    deallocate(VDEL)
  end if 

end subroutine xfoil_cleanup


!------------------------------------------------------------------------------
! Init Boundary layer of xfoil viscous calculation  
!------------------------------------------------------------------------------
subroutine xfoil_init_BL (show_details)

  use xfoil_inc, only : LIPAN, LBLINI

  logical, intent(in) :: show_details

  LIPAN  = .false.
  LBLINI = .false.

  if(show_details) call print_colored (COLOR_NOTE, 'i')

end subroutine xfoil_init_BL 

!------------------------------------------------------------------------------
! Scale max thickness and camber and their positions of foil 
!        using xfoil THKCAM and HIPNT
!
!   f_thick  - scaling factor for thickness
!   d xthick - delta x for max thickness x-position
!   f_camb   - scaling factor for camber
!   d_xcamb  - delta x for max camber position
!
! 
! ** Note ** 
!
! Before calling this subroutine, "smooth_paneling()" (which uses xfoil PANGEN)
! should be done on foil to avoid strange artefacts at the leading edge.
! XFOIL>HIPNT (moving thickness highpoint) is very sensible and behaves badly
! if the LE curvature does not fit to the spline algorithm
!------------------------------------------------------------------------------

subroutine xfoil_scale_thickness_camber (infoil, f_thick, d_xthick, f_camb, d_xcamb, outfoil)

  use xfoil_inc, only : AIJ

  type(airfoil_type), intent(in)  :: infoil
  type(airfoil_type), intent(out) :: outfoil
  double precision, intent(in) :: f_thick, d_xthick, f_camb, d_xcamb
  double precision :: thick, xthick, camb, xcamb

! Check to make sure xfoil is initialized
  if (.not. allocated(AIJ)) then
    call my_stop ("xfoil is not initialized")
  end if
! Set xfoil airfoil and prepare globals, get current thickness
  call xfoil_set_airfoil (infoil)
  call xfoil_get_geometry_info  (thick, xthick, camb, xcamb) 


! Run xfoil to change thickness and camber and positions

  IF ((f_thick /= 1.d0) .or. (f_camb /= 1.d0))  &
    call THKCAM (f_thick, f_camb)

  IF ((d_xcamb /= 0.d0) .or. (d_xthick /= 0.d0))  then
    call HIPNT  (xcamb + d_xcamb, xthick + d_xthick)
    call correct_HIPNT_artefacts (infoil)
  end if 
        
! retrieve outfoil from xfoil buffer

  call xfoil_reload_airfoil(outfoil)

end subroutine xfoil_scale_thickness_camber


!------------------------------------------------------------------------------
! Set max thickness and camber and their positions of foil 
!        using xfoil THKCAM and HIPNT
!
!   maxt  - new thickness
!   xmaxt - new max thickness x-position
!   maxc  - new camber
!   xmaxc - new max camber position
!
!   if one of the values = 0.0 then this value is not set
! 
! ** Note ** 
!
! Before calling this subroutine, "smooth_paneling()" (which uses xfoil PANGEN)
! should be done on foil to avoid strange artefacts at the leading edge.
! XFOIL>HIPNT (moving thickness highpoint) is very sensible and behaves badly
! if the LE curvature does not fit to the spline algorithm
!------------------------------------------------------------------------------
subroutine xfoil_set_thickness_camber (infoil, maxt, xmaxt, maxc, xmaxc, outfoil)

  use xfoil_inc, only : AIJ

  type(airfoil_type), intent(in)  :: infoil
  type(airfoil_type), intent(out) :: outfoil

  double precision, intent(in) :: maxt, xmaxt, maxc, xmaxc
  double precision :: CFAC,TFAC, thick, xthick, camb, xcamb, old_te_gap

! Check to make sure xfoil is initialized
  if (.not. allocated(AIJ)) then
    call print_error ("Set thickness or camber: xfoil is not initialized!  Call xfoil_init() first.")
    stop
  end if

! Sanity Checks

  if (maxt > 0d0) then
    if (maxt < 0.01d0 .or. maxt > 0.2d0) then 
      call print_error ("Set thickness or camber: Thickness must be between 0.01 and 0.2", 3)
      outfoil = infoil
      return 
    end if 
  end if  

  if (xmaxt > 0d0) then
    if (xmaxt < 0.1d0 .or. xmaxt > 0.9d0) then 
      call print_error ("Set thickness or camber: Max thickness location must be between 0.1 and 0.9", 3)
      outfoil = infoil
      return 
    end if 
  end if  

  if (maxc > 0d0) then
    if (maxc > 0.1d0) then 
      call print_error ("Set thickness or camber: Camber must be less than 0.1", 3)
      outfoil = infoil
      return 
    end if 
  end if  

  if (xmaxc > 0d0) then
    if (xmaxc < 0.1d0 .or. xmaxc > 0.9d0) then 
      call print_error ("Set thickness or camber: Max camber location must be between 0.1 and 0.9", 3)
      outfoil = infoil
      return 
    end if 
  end if  

  old_te_gap = get_te_gap (infoil)

! Set xfoil airfoil and prepare globals, get current thickness
  call xfoil_set_airfoil (infoil)
  call xfoil_get_geometry_info  (thick, xthick, camb, xcamb) 

! Run xfoil to change thickness and camber 
  CFAC = 1.0
  TFAC = 1.0

  if (maxc > 0.0d0) then
    IF(camb .NE.0.0 .AND. maxc.NE.999.0) CFAC = maxc / camb
  end if
  if (maxt > 0.0d0) then
    IF(thick.NE.0.0 .AND. maxt.NE.999.0) TFAC = maxt / thick
  end if 

  call THKCAM ( TFAC, CFAC)

! Run xfoil to change highpoint of thickness and camber 

  if((xmaxc > 0d0) .or. (xmaxt > 0d0)) then
    call HIPNT (xmaxc, xmaxt)
    call correct_HIPNT_artefacts (infoil)
  end if 

! THKCAM may have changed te gap 

  if (old_te_gap /= xfoil_te_gap()) then 
  ! Run xfoil to set TE gap 
    call TGAP(old_te_gap,0.8d0)
    write (*,*) "THKCAM TE change encountered: te old ", old_te_gap, "   te new ", xfoil_te_gap()
  end if 

! retrieve outfoil from xfoil buffer
  call xfoil_reload_airfoil(outfoil)

end subroutine xfoil_set_thickness_camber


!------------------------------------------------------------------------------
! Corrects artefacts of xfoils HIPNT (change max thickness or camper position) 
!
! - de-rotate if needed
! - set LE to 0,0 
! - set TE to old value
!     
! In:
!   infoil      - foil prior to HIPNT 
! Modifies:
!   xfoil buffer airfoil
!------------------------------------------------------------------------------
subroutine correct_HIPNT_artefacts (infoil)

  use xfoil_inc, only : XB, YB, NB

  type(airfoil_type), intent(in) :: infoil

  integer           :: i
  double precision, parameter ::EPSILON = 1d-10   ! ... when coordinate will be 0.0
  double precision  :: in_angle, out_angle, cosa, sina


! HIPNT may have changed LE from 0,0 
  do i = 1, NB
    if ((XB(i) == 0d0 ) .and. &
        ((abs(YB(i)) < EPSILON ) .and. ( abs(YB(i)) /= 0d0 ))) then 
      ! write (*,*) "HIPNT LE change encountered: ", i, XB(i), YB(i)
      XB(i) = 0d0
      YB(i) = 0d0
    end if 
  end do   

! HIPNT may have rotated the airfoil - 
! Rotate the airfoil so chord is on x-axis 

  in_angle  = atan2 ((infoil%y(1) +infoil%y(NB ))/2.d0,(infoil%x(1) +infoil%x(NB) )/2.d0)
  out_angle = atan2 ((YB(1)+YB(NB))/2.d0,(XB(1)+XB(NB))/2.d0)
  if (in_angle /= out_angle) then
    !write (*,*) "HIPNT rotated airfoil: before ",in_angle, "   after ", out_angle
  
    cosa  = cos (in_angle - out_angle) 
    sina  = sin (in_angle - out_angle) 
    do i = 1, NB
      ! outfoil%x(i) = outfoil%x(i) * cosa - outfoil%y(i) * sina
      YB(i) = XB(i) * sina + YB(i) * cosa
    end do
  end if 

! HIPNT may have changed TE from 0,0 
  if (infoil%y(1)  /= YB(1)) then
    !write (*,*) "HIPNT upper TE change: z before ",infoil%y(1), "   after ", YB(1)
    YB(1)   = infoil%y(1) 
  end if 
  if (infoil%y(NB) /= YB(NB)) then 
    !write (*,*) "HIPNT lower TE change: z before ",infoil%y(NB), "   after ", YB(NB)
    YB(NB)  = infoil%y(NB)
  end if 

end subroutine correct_HIPNT_artefacts 


!------------------------------------------------------------------------------
! Scale LE radius 
!        using xfoil LERAD
! In:
!   infoil      - foil to scale LE
!   f_radius    - scaling factor for LE radius
!   x_blend     - blending distance/c from LE
! Out:
!   new_radius  - new LE radius
!   outfoil     = modified foil
! 
! ** Note ** 
!
! Before calling this subroutine, "smooth_paneling()" (which uses xfoil PANGEN)
! should be done on foil to avoid strange artefacts at the leading edge.
!------------------------------------------------------------------------------
subroutine xfoil_scale_LE_radius (infoil, f_radius, x_blend, outfoil)

  use xfoil_inc, only : AIJ, RADBLE

  type(airfoil_type), intent(in)  :: infoil
  double precision, intent(in) :: f_radius, x_blend
  double precision  :: new_radius
  type(airfoil_type), intent(out) :: outfoil

! Check to make sure xfoil is initialized
  if (.not. allocated(AIJ)) then
    call my_stop ("xfoil is not initialized.")
  end if

! Set xfoil airfoil and prepare globals, get current thickness
  call xfoil_set_airfoil (infoil)

! Run xfoil to change thickness and camber and positions
  IF ((f_radius /= 1.d0))  call LERAD (f_radius,x_blend, new_radius) 

! Update xfoil globals
  RADBLE = new_radius

  call xfoil_reload_airfoil(outfoil)

end subroutine xfoil_scale_LE_radius



!------------------------------------------------------------------------------
! Set trailing edge TE gap using xfoil TGAP  
!        
! In:
!   infoil      - foil to set gap 
!   te_gap      - new te gap in y coordinates
!   x_blend     - blending distance/c from LE 0...1
! Out:
!   outfoil     = modified foil
! 
!------------------------------------------------------------------------------
subroutine xfoil_set_te_gap (infoil, te_gap, x_blend, outfoil)

  use xfoil_inc, only : AIJ

  type(airfoil_type), intent(in)  :: infoil
  double precision, intent(in) :: te_gap, x_blend
  type(airfoil_type), intent(out) :: outfoil

  integer         :: ile

  outfoil = infoil 

! Check to make sure xfoil is initialized
  if (.not. allocated(AIJ)) then
    call my_stop ("xfoil is not initialized.")
    stop
  end if

! Set xfoil airfoil and prepare globals, get current thickness
  call xfoil_set_airfoil (infoil)

  if (te_gap < 0d0 .or. te_gap > 0.05d0) then
    call print_error ("TE gap must be between 0.0 and 0.05")
  elseif (x_blend < 0d0 .or. x_blend > 1d0) then
    call print_error ("TE gap blending range must be between 0.0 and 1.0")
  else

    ile = minloc (infoil%x, 1)
    if (ile == 0 .or. infoil%x(ile) /= 0d0) then 
      call my_stop ("set_te_gap: Leading edge isn't at 0,0")
    end if  

  ! Run xfoil to set TE gap 
    call TGAP(te_gap,x_blend)
    call xfoil_reload_airfoil(outfoil)

  ! TGAP could have changed leading edge a very little ...

    outfoil%x(ile) = 0d0
    outfoil%y(ile) = 0d0

  end if 

end subroutine xfoil_set_te_gap


function get_te_gap (foil)

  type(airfoil_type), intent(in)  :: foil
  double precision :: get_te_gap

  get_te_gap = sqrt ((foil%x(1) - foil%x(size(foil%x)))**2 + &
                     (foil%y(1) - foil%y(size(foil%y)))**2)
end function 

!------------------------------------------------------------------------------
! calculat te gap of xfoil buffer airfoil 
!------------------------------------------------------------------------------

function xfoil_te_gap ()

  use xfoil_inc, only : XB, YB, NB
  double precision :: xfoil_te_gap

  xfoil_te_gap = sqrt ((XB(1) - XB(NB))**2 + &
                     (YB(1) - YB(NB))**2)

end function xfoil_te_gap


!-------------------------------------------------------------------------
! gets buffer airfoil thickness, camber .. positions
!-------------------------------------------------------------------------
subroutine xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc) 
 
  use xfoil_inc
  double precision, intent(out) :: maxt, xmaxt, maxc, xmaxc

  Real*8 :: rxmaxt, rmaxc, rxmaxc
  Real*8 :: TYMAX
  
! find the current buffer airfoil camber and thickness

  CALL GETCAM(XCM,YCM,NCM,XTK,YTK,NTK,                  &
              XB,XBP,YB,YBP,SB,NB )
  CALL GETMAX(XCM,YCM,YCMP,NCM,rxmaxc,rmaxc)
  CALL GETMAX(XTK,YTK,YTKP,NTK,rxmaxt,TYMAX)

  maxt  =  2d0 * DBLE (TYMAX)
  xmaxt = DBLE (rxmaxt)
  maxc  = DBLE (rmaxc)
  xmaxc = DBLE (rxmaxc)

! correct max camber position for camber = 0 as it would be xfoil "random"

  if (maxc < 0.0001d0) xmaxc = 0d0

end subroutine xfoil_get_geometry_info



subroutine xfoil_le_find (foil, xle, yle)

  !! find the 'real' leading edge on spline 
  !! (normal vector at LE hits trailing edge)


  type(airfoil_type), intent(in) :: foil
  double precision, intent(out)  :: xle, yle

  double precision, allocatable :: s (:), xp(:), yp(:)
  double precision  :: sle
  integer           :: npt 

  interface
    double precision function SEVAL(SS, X_dum, XS, S_dum, N)
      integer, intent(in) :: N
      double precision, intent(in) :: SS
      double precision, dimension(N), intent(in) :: X_dum, XS, S_dum
    end function SEVAL
  end interface 

  npt = size(foil%x)

  allocate(s(npt))
  allocate(xp(npt))
  allocate(yp(npt))

  call SCALC  (foil%x, foil%y, s, npt)
  call SEGSPL (foil%x, xp, s, npt)
  call SEGSPL (foil%y, yp, s, npt)
  
  call LEFIND (sle, foil%x, xp, foil%y, yp, s, npt, .true.)

  xle = SEVAL (sle, foil%x, xp, s, npt)
  yle = SEVAL (sle, foil%y, yp, s, npt)

end subroutine

!-------------------------------------------------------------------------
! Reloads airfoil from xfoil buffer foil
!-------------------------------------------------------------------------
subroutine xfoil_reload_airfoil(foil)

  use xfoil_inc, only : XB, YB, NB

  type(airfoil_type), intent(inout) :: foil

  if (allocated (foil%x))  deallocate (foil%x)
  if (allocated (foil%y))  deallocate (foil%y)
  allocate(foil%x(NB))
  allocate(foil%y(NB))

  foil%npoint = NB
  foil%x = XB(1:NB)
  foil%y = YB(1:NB)
  
end subroutine xfoil_reload_airfoil

!--JX-mod  --------------------------------------------------------------------
! 
!  Toolfunctions to handle out lier (flip) detection of drag and lift 
!
!------------------------------------------------------------------------------

subroutine init_statistics (npoints)

  integer, intent (in) :: npoints 
  integer :: i
  
  if (allocated(drag_stats))  deallocate (drag_stats)

  allocate (drag_stats(npoints))
  do i = 1, npoints
    drag_stats(i)%nvalue   = 0
    drag_stats(i)%no_check = .false.
    drag_stats(i)%minval   = 0.d0
    drag_stats(i)%maxval   = 0.d0
    drag_stats(i)%meanval  = 0.d0
  end do 

end subroutine init_statistics


!------------------------------------------------------------------------------
subroutine update_statistic (iop, new_value)

  doubleprecision, intent (in) :: new_value 
  integer, intent (in)         :: iop
  integer                      :: nmean

  if (.not. allocated (drag_stats)) return

!$omp critical (update)
  
  if (drag_stats(iop)%nvalue == 0) then      ! first value is best we have (could be outlier!)

    drag_stats(iop)%minval  = new_value 
    drag_stats(iop)%maxval  = new_value 
    drag_stats(iop)%meanval = new_value 
    drag_stats(iop)%nvalue  =  1

  elseif (drag_stats(iop)%nvalue == 1 .and. &
          new_value < (drag_stats(iop)%maxval / 2d0)) then ! first value was probably outlier
    drag_stats(iop)%minval  = new_value 
    drag_stats(iop)%maxval  = new_value
    drag_stats(iop)%meanval = new_value            ! reset statistics
    drag_stats(iop)%nvalue  = 1

  elseif (.not. is_out_lier (iop, new_value)) then   ! normal handling
    drag_stats(iop)%minval  = min (drag_stats(iop)%minval, new_value) 
    drag_stats(iop)%maxval  = max (drag_stats(iop)%maxval, new_value)
    nmean = min (drag_stats(iop)%nvalue, 50)    ! take max 50 values for mean, so it may develop...
    drag_stats(iop)%meanval = (drag_stats(iop)%meanval * nmean + new_value) / (nmean + 1)
    drag_stats(iop)%nvalue  = drag_stats(iop)%nvalue + 1
  end if 
  

!$omp end critical (update)

end subroutine update_statistic



!------------------------------------------------------------------------------
function is_out_lier (iop, check_value) 

  doubleprecision, intent (in) :: check_value
  integer, intent (in)         :: iop
  logical :: is_out_lier 

  doubleprecision ::  dev_from_mean, max_meanval

  is_out_lier = .false. 

  if (.not. allocated (drag_stats)) return

  if(drag_stats(iop)%nvalue > 0 .and. (.not. drag_stats(iop)%no_check)) then           !do we have enough values to check? 

    dev_from_mean    = abs(check_value - drag_stats(iop)%meanval)/max(0.0001d0, drag_stats(iop)%meanval) 

    if (dev_from_mean > 0.4d0   .or. &                          ! deviation from mean > 40%? 
        check_value   <  (drag_stats(iop)%minval / 2d0) .or. &  ! half of minval? 
        check_value   >  (drag_stats(iop)%maxval * 2d0)) then   ! double of maxval? 

      is_out_lier = .true.

    !  call show_out_lier (iop, check_value) 
    
    end if

! In case of polar generation we have no values for iop  (every op is unique) 
!  -> is check_value greater than 8 times (cd) value of other op_points? (empirical)    

  elseif (drag_stats(iop)%nvalue == 0 ) then        

    max_meanval = maxval (drag_stats%meanval)
    if (max_meanval > 0d0) then
      if  (check_value > (8d0 * max_meanval)) then 
        is_out_lier = .true.
      end if 
    end if 

  end if 

end function is_out_lier


!------------------------------------------------------------------------------
subroutine show_out_lier (iop, check_value)

  doubleprecision, intent (in) :: check_value
  integer, intent (in) :: iop
  character (10)       :: text

  write (text,'(I10)') drag_stats(iop)%nvalue
  write (*,'(//,10x, "Outlier detect for op",I2,4x,"cd =", F7.4)', advance='no') iop, check_value
  write (*,'(2x,"Min",F7.4,"  Mean", F7.4, "  Max",F7.4, "   nstat_values ",A)') &
      drag_stats(iop)%minval, drag_stats(iop)%meanval, drag_stats(iop)%maxval, &
      trim(adjustl(text))
  write (*,*)

end subroutine show_out_lier


!----Check if lift has changed although it should be fix with spec_cl ---------
function cl_changed (spec_cl, op_point, cl)

  doubleprecision, intent (in) :: op_point, cl
  logical, intent (in) :: spec_cl
  logical :: cl_changed

  if (spec_cl .and. (abs(cl - op_point) > 0.01d0)) then
    cl_changed = .true.
  else
    cl_changed = .false.
  end if 

end function cl_changed

end module xfoil_driver



