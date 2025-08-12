! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2020-2025 Jochen Guenzel

module xfoil_driver


  ! driver to use xfoil to analyze an airfoil

  use os_util 
  use print_util
  use airfoil_base,      only : airfoil_type


  implicit none

  type re_type 
    double precision :: number            ! Reynolds Number
    integer          :: type              ! Type 1 or 2 (fixed lift)
  end type re_type

  ! Hold result of xfoil boundary layer (BL) infos of an op_pooint

  type bubble_type      
    logical          :: found = .false.                   ! a bubble was detected           
    double precision :: xstart = 0d0                      ! start of separation: CF (shear stress) < 0
    double precision :: xend = 0d0                        ! end   of separation: CF (shear stress) > 0
  end type bubble_type                              

  ! defines an op_point for xfoil calculation
 
  type op_point_spec_type  
  
    ! aero - xfoil 
    logical                     :: spec_cl                 ! op based on alpha or cl
    double precision            :: value                   ! base value of cl or alpha
    type (re_type)              :: re, ma                  ! Reynolds and Mach 
    double precision            :: ncrit                   ! xfoil ncrit

    ! flap setting
    double precision            :: flap_angle              ! fix or initial  flap angle of this op op point 
    logical                     :: flap_optimize           ! optimize flap angle

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
    double precision :: x_flap, y_flap 
    character(3)     :: y_flap_spec
    double precision :: min_flap_angle
    double precision :: max_flap_angle
    integer          :: ndv                       ! no of design variables = no of flaps to optimize
    double precision, allocatable :: start_flap_angle(:) ! start angle of optized flaps  
  end type flap_spec_type


  ! Hold result of xfoil aero calculation of an op_pooint

  type op_point_result_type                              
    logical                     :: converged                ! did xfoil converge? 
    double precision            :: cl                       ! lift coef.  - see also spec_cl
    double precision            :: alpha                    ! alpha (aoa) - see also spec_cl
    double precision            :: cd                       ! drag coef.  
    double precision            :: cdp                      ! pressure drag coef.  
    double precision            :: cm                       ! moment coef. 
    double precision            :: xtrt                     ! point of transition - top side 
    double precision            :: xtrb                     ! point of transition - bottom side 
    type (bubble_type) :: bubblet, bubbleb                  ! bubble info - top and bottom 

  end type op_point_result_type                              


  ! defines xfoils calculation environment - will be initialized in input_read

  type xfoil_options_type
    double precision :: ncrit                               ! Critical ampl. ratio
    double precision :: xtript, xtripb                      ! forced trip locations
    logical :: viscous_mode                                 ! do viscous calculation           
    logical :: silent_mode                                  ! Toggle xfoil screen write
    logical :: show_details                                 ! show some user entertainment 
    integer :: maxit                                        ! max. iterations for BL calcs
    double precision :: vaccel                              ! xfoil BL convergence accelerator
    logical :: fix_unconverged = .false.                    ! try to fix unconverged pts.
    logical :: detect_outlier = .false.                     ! try to detect op point outlier during optimization
    logical :: detect_bubble = .false.                      ! do shear stress bubble detection 
    logical :: repair_polar_outlier = .false.               ! try to remove outlier in polar generation
    logical :: exit_if_unconverged = .false.                ! exit op points loop if a point is unconverged
    logical :: exit_if_clmax = .false.                      ! exit op points loop if crossed max cl (polar generation) 
    integer :: exit_if_clmax_nops = 3                       !  ... n op points behind cl max        (polar generation) 
    logical :: reinitialize = .false.                       ! reinitialize BLs per op_point
  end type xfoil_options_type



  ! --- static, private ------------------------------------------

  ! result statistics for drag outlier detetction 

  type outlier_entry_type   
    logical :: no_check                ! deactivate detection e.g. when flaps are set
    integer :: nvalue                  ! total numer of values tested
    double precision :: minval         ! the smallest value up to now 
    double precision :: maxval         ! the biggest value up to now 
    double precision :: meanval        ! the average value up to now 
  end type outlier_entry_type

  type (outlier_entry_type), dimension(:), allocatable, private :: outlier_stats


 ! xfoil run  statistics 

  type xfoil_statistics_type    
    integer                   :: ncalc = 0            ! total number of viscalc 
    integer                   :: ncalc_not_conv = 0   ! number of not  converged viscalc 
    integer                   :: it_viscal = 0        ! time ticks spent in viscal  
    integer                   :: it_specal = 0        ! time ticks spent in specal  
    integer                   :: it_speccl = 0        ! time ticks spent in speccl  
    integer                   :: nretry_ok = 0        ! number or successful retries 
    integer                   :: nretry_failed = 0    ! number or successful retries 
    integer                   :: noutlier = 0         ! number or outlier  
  end type xfoil_statistics_type

  type (xfoil_statistics_type), private     :: stats 


contains


  subroutine run_op_points (foil, xfoil_options, flap_spec, flap_angle, &
                            op_points_spec, op_points_result)

    !----------------------------------------------------------------------------
    !! Core routine to run xfoil calculation for each operating points
    !!      returns Cl, Cd, Cm, ... for an airfoil
    !----------------------------------------------------------------------------

    use xfoil_inc    

    type(airfoil_type), intent(in)                        :: foil
    type(xfoil_options_type), intent(in)                  :: xfoil_options
    type(flap_spec_type), intent(in)                      :: flap_spec
    type(op_point_spec_type), intent(in)                  :: op_points_spec (:)
    type(op_point_result_type), allocatable, intent(out)  :: op_points_result (:)
    double precision, intent(in)                          :: flap_angle (:)

    integer                     :: i, noppoint, nops_behind_clmax
    double precision            :: prev_op_delta, op_delta, prev_flap_angle, prev_op_spec_value
    logical                     :: point_fixed, show_details, flap_changed, prev_op_spec_cl
    logical                     :: detect_outlier
    type(op_point_spec_type)    :: op_spec
    type(op_point_result_type)  :: op, prev_op


    noppoint = size(op_points_spec,1) 
    allocate (op_points_result(noppoint))

    ! Sanity checks

    if (noppoint == 0) then                           ! nothing to do 
      return
    end if
    if (.not. allocated(AIJ)) then
      call my_stop ("xfoil is not initialized.")
    end if

    ! Init variables

    op_points_result%converged = .false.          ! init - watch "early exit" 
    op_points_result%cl        = 0d0  
    op_points_result%cd        = 0d0  
    op_points_result%alpha     = 0d0  
    op_points_result%cm        = 0d0             
    op_points_result%xtrt      = 0d0               
    op_points_result%xtrb      = 0d0              
    
    prev_op_delta   = 0d0
    prev_op_spec_cl = op_points_spec(1)%spec_cl
    prev_flap_angle = 999d0 
    prev_op         = op_points_result(1)
    flap_changed    = .false.

    show_details   = xfoil_options%show_details
    detect_outlier = xfoil_options%detect_outlier
    nops_behind_clmax = 0 

    ! init statistics for out lier detection the first time and when polar changes

    if (detect_outlier) then 
      call init_outlier_ifNeeded (noppoint)
    end if


    ! Set default Xfoil parameters 

    call xfoil_defaults(xfoil_options)


    if (show_details) then 
      call print_text ('Xfoil evaluate op points: ', 5, no_crlf=.true.)
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

      !  print newline if output gets too long
      if (show_details .and.( mod(i,80) == 0)) write (*,'(/,7x,A)',advance = 'no') '       '

      ! if flpas are activated, check if the angle has changed to reinit foil

      if(flap_spec%use_flap .and. (flap_angle(i) /= prev_flap_angle)) then 
        flap_changed = .true.
        prev_flap_angle = flap_angle(i)
        if (allocated(outlier_stats)) outlier_stats%no_check = .true.  !deactivate outlier detection when flapping
      else
        flap_changed = .false.
      end if 

      ! set airfoil, apply flap deflection, init BL if needed

      if (flap_changed .or. (i == 1)) then

        call xfoil_set_airfoil(foil)              

        if (flap_changed) then                              ! apply flap only if set to non zero degrees
          call xfoil_apply_flap_deflection(flap_spec, flap_angle(i))
        end if     

        call xfoil_init_BL (show_details)                   ! in case of flaps (or first op) always init boundary layer 

      else

        if (xfoil_options%reinitialize) then                ! Init BL always if set in parameters 
          call xfoil_init_BL (show_details)
        else
          if (op_spec%spec_cl .neqv. prev_op_spec_cl) then  ! init if op_mode changed
            call xfoil_init_BL (show_details)
            prev_op_delta = 0d0
          else                                              ! Init BL if the direction of alpha or cl changes 
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


      ! Now finally run xfoil at op_point

      call run_op_point (op_spec, xfoil_options, show_details, op)

      ! Handling of unconverged points

      if (op%converged) then
        if (detect_outlier .and. is_outlier (i, op%cd)) then
          op%converged = .false.
          if (show_details) call print_colored (COLOR_WARNING, 'flip')
        else if (cl_changed (op_spec%spec_cl, op_spec%value, op%cl)) then
          op%converged = .false.
          if (show_details) call print_colored (COLOR_WARNING, 'lift')
        end if 
      end if

      if (.not. op%converged .and. xfoil_options%fix_unconverged) then

        call try_fix_unconverged (show_details, i, op_spec, xfoil_options, op, point_fixed)

        ! no fix achieved - reinit BL (for the next op) - set converged flag to .false.
        if(.not. point_fixed) then
          if (.not. xfoil_options%reinitialize) call xfoil_init_BL (show_details)
          op%converged = .false.
        end if
      end if

      op_points_result(i) = op

      ! early exit if not converged for speed optimization 

      if ((.not. op%converged) .and. xfoil_options%exit_if_unconverged) exit

      ! polar generation: exit if we crossed cl max  

      if (xfoil_options%exit_if_clmax) then 
        ! rare pathologic case - cd close to 0.0 
        if (op%cd < 0.00001d0) then 
          op_points_result(i)%converged = .false. 
          exit
        end if 

        ! check if end of polar reached 
        if (op_spec%spec_cl) then 
          if (op%converged) then
            nops_behind_clmax = 0 
          else
            nops_behind_clmax = nops_behind_clmax + 1
          end if 
        else
          call check_clmax_crossed (prev_op, op, nops_behind_clmax)
        end if 
        if (nops_behind_clmax >= xfoil_options%exit_if_clmax_nops) exit

      end if 

      ! Update statistics

      if (op%converged) then 

        if (detect_outlier) call update_outlier_stats (i, op%cd)      ! Update cd outlier statistics

        ! Support Type 1 and 2 re numbers - cl may not be negative  
        if ((op_spec%re%type == 2) .and. (op%cl <= 0d0)) then 
          write(*,'(15x,A,I2,A, F6.2)') "Warning: Negative lift for Re-Type 2 at" // &
          " op",i," - cl:",op%cl
        end if 
      end if 
  
    end do 

    ! optional - repair polar outlier 

    if (xfoil_options%repair_polar_outlier) then 
      call repair_polar_outlier (op_points_spec, op_points_result)
    end if  
  
    if(show_details) print *
    
  end subroutine run_op_points


  subroutine check_clmax_crossed (prev_op, op, nops_behind_clmax)

    !----------------------------------------------------------------------------
    !! Try to check if current op is nops behind cl max during polar generation 
    !! - for positive alpha: cl max reached when cl is decreasing again 
    !! - for negative alpha: cl max reached when cl/cd is increasing again 
    !----------------------------------------------------------------------------

    type(op_point_result_type), intent(in)    :: op
    type(op_point_result_type), intent(inout) :: prev_op
    integer, intent(inout)                    :: nops_behind_clmax

    logical           :: going_up
    double precision  :: prev_clcd, clcd 

    if (.not. op%converged) then
      nops_behind_clmax =  nops_behind_clmax + 1
    else 

      ! prepare glide 

      if (prev_op%cd /= 0d0) then 
        prev_clcd = prev_op%cl / prev_op%cd
      else 
        prev_clcd = 0d0
      end if 
      if (op%cd /= 0d0) then 
        clcd = op%cl / op%cd
      else 
        clcd = 0d0
      end if 


      going_up = (op%alpha - prev_op%alpha) > 0d0 

      ! increasing alpha - going up - check cl max crossed 

      if (going_up) then 

        if (op%cl < prev_op%cl) then 
          nops_behind_clmax =  nops_behind_clmax + 1
        else 
          nops_behind_clmax =  0                              ! reset crossed counter       
        end if 

        ! special case cl and cl/cd are less than previous 
        if ((op%cl < prev_op%cl) .and. (clcd < prev_clcd)) then 
          nops_behind_clmax =  nops_behind_clmax + 10         ! hacky - force end 
        end if 

      ! decreasing alpha - going down - check clcd (glide) min crossed 

      else

        if ((clcd > prev_clcd)) then 
          nops_behind_clmax =  nops_behind_clmax + 1
        else 
          nops_behind_clmax =  0 
        end if 
      end if 

      prev_op = op                                              ! remind prev op if it is converged 

    end if 

  end subroutine



  subroutine try_fix_unconverged (show_details, iop, op_spec, xfoil_options, op, point_fixed)

    !----------------------------------------------------------------------------
    !! Try to fix unconverged operating point by 
    !! - increasing / decreasing spec_value of op point a little 
    !! - increasing re number a little 
    !----------------------------------------------------------------------------

    type(op_point_spec_type), intent(in)    :: op_spec
    type(xfoil_options_type), intent(in)    :: xfoil_options
    type(op_point_result_type), intent(out) :: op
    logical, intent(in)                     :: show_details 
    integer, intent(in)                     :: iop 
    logical, intent(out)                    :: point_fixed
    
    type(xfoil_options_type)    :: tmp_xfoil_options
    type(op_point_spec_type)    :: tmp_op_spec
    type(op_point_result_type)  :: tmp_op

    integer                     :: iretry, nretry
    logical                     :: detect_outlier 

    detect_outlier = xfoil_options%detect_outlier
    tmp_xfoil_options = xfoil_options

    if (show_details) call print_colored (COLOR_NOTE, '[')

    ! Try to initialize BL at intermediate new point (in the direction away from stall)

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

    tmp_xfoil_options%maxit = xfoil_options%maxit + (xfoil_options%maxit/3)   ! increase max iterations

    call run_op_point  (tmp_op_spec, tmp_xfoil_options, show_details , tmp_op)

    ! If this intermediate point converged
    !    try to run again at the old operating point decreasing RE a little ...

    point_fixed = .false.

    if (tmp_op%converged) then 

      iretry = 1
      nretry = 2                ! try more
      tmp_op_spec = op_spec

      do while (.not. point_fixed .and. (iretry <= nretry)) 

        tmp_op_spec%re%number = op_spec%re%number * 0.996d0  !  result will be 'worse' 

        if (xfoil_options%reinitialize) call xfoil_init_BL (.false.)

        ! increase iterations in the second try 
        if (iretry == 1) then 
          tmp_xfoil_options%maxit = xfoil_options%maxit
        else
          tmp_xfoil_options%maxit = xfoil_options%maxit + (xfoil_options%maxit / 2)  
        end if 
         
        call run_op_point (tmp_op_spec, tmp_xfoil_options, show_details, op)
                              
        if (.not. op%converged .or. (detect_outlier .and. is_outlier (iop, op%cd))  &
            .or. (cl_changed (tmp_op_spec%spec_cl, tmp_op_spec%value, op%cl))) then 

          call xfoil_init_BL (show_details .and. (.not. xfoil_options%reinitialize))
          !$omp atomic
          stats%nretry_failed = stats%nretry_failed + 1

        else 
          point_fixed = .true.
          !$omp atomic
          stats%nretry_ok = stats%nretry_ok + 1
        end if 

        iretry = iretry + 1

      end do
    else 
      ! retry with new value and re already failed 
      !$omp atomic
      stats%nretry_failed = stats%nretry_failed + 1

    end if  

    if(show_details) then 
      write (*,'(A)',advance = 'no') ']'
      if (point_fixed) then 
        call print_colored (COLOR_NOTE, 'fixed ')
      else
        call print_colored (COLOR_ERROR,  'x')
      end if  
    end if 
    

  end subroutine 



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

subroutine run_op_point (op_point_spec, xfoil_options, show_details, op_point_result)

  use xfoil_inc

  type(op_point_spec_type),         intent(in)  :: op_point_spec
  type(xfoil_options_type),         intent(in)  :: xfoil_options
  logical,                          intent(in)  :: show_details
  type(op_point_result_type),       intent(out) :: op_point_result

  logical             :: viscous_mode, detect_bubble
  integer             :: niter_needed, maxit
  double precision    :: save_ACRIT
  ! integer             :: itime_start, itime_speccl, itime_specal, itime_viscal

  maxit         = xfoil_options%maxit
  viscous_mode  = xfoil_options%viscous_mode
  detect_bubble = xfoil_options%detect_bubble

  op_point_result%cl    = 0.d0
  op_point_result%cd    = 0.d0
  op_point_result%alpha = 0.d0
  op_point_result%cm    = 0.d0
  op_point_result%xtrt  = 0.d0
  op_point_result%xtrb  = 0.d0
  op_point_result%converged = .true.

  ! statistics 
  ! call system_clock(itime_start)
  ! itime_speccl = itime_start
  ! itime_specal = itime_start
  ! itime_viscal = itime_start
 

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
    ! call system_clock(itime_speccl)

  else 
    LALFA = .TRUE.
    ALFA = op_point_spec%value * DTOR

    call SPECAL
    ! call system_clock(itime_specal)

  end if

  if (abs(ALFA-AWAKE) .GT. 1.0E-5) LWAKE  = .false.
  if (abs(ALFA-AVISC) .GT. 1.0E-5) LVCONV = .false.
  if (abs(MINF-MVISC) .GT. 1.0E-5) LVCONV = .false.

  ! Viscous calculations (if requested)

  op_point_result%converged = .true. 

  if (viscous_mode) then 
    
    call VISCAL(maxit, niter_needed)

    ! call system_clock(itime_viscal)

    ! converged? 

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
    op_point_result%cdp  = CDP 
    op_point_result%xtrt = XOCTR(1)
    op_point_result%xtrb = XOCTR(2)
    if (op_point_result%converged .and. xfoil_options%detect_bubble) then 
      call detect_bubble_top_bot (op_point_result%bubblet, op_point_result%bubbleb)
    end if
  else
    op_point_result%cd   = CDP
    op_point_result%cdp  = CDP 
    op_point_result%xtrt = 0.d0
    op_point_result%xtrb = 0.d0
  end if

! Final check for NaNs

  if (isnan(op_point_result%cl)) then
    op_point_result%cl = -1.D+08
    op_point_result%converged = .false.
  end if
  if (isnan(op_point_result%cd)) then
    op_point_result%cd  = 1.D+08
    op_point_result%cdp = 1.D+08
    op_point_result%converged = .false.
  end if
  if (isnan(op_point_result%cm)) then
    op_point_result%cm = -1.D+08
    op_point_result%converged = .false.
  end if

  if(show_details) then 
    if (op_point_result%converged) then
      call print_colored (COLOR_NOTE,  '.')
    else
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

  ! update statistics

  !$omp critical
  stats%ncalc = stats%ncalc + 1
  if (.not. op_point_result%converged) then
    stats%ncalc_not_conv = stats%ncalc_not_conv + 1
  end if 
  ! time measurements 
  ! stats%it_specal = stats%it_specal + itime_specal - itime_start
  ! stats%it_speccl = stats%it_speccl + itime_speccl - itime_start
  ! stats%it_viscal = stats%it_viscal + itime_viscal - itime_start !max(itime_speccl, itime_specal) 
  !$omp end critical


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

subroutine detect_bubble_top_bot (bubblet, bubbleb)

  use xfoil_inc

  type (bubble_type), intent(inout) :: bubblet, bubbleb
  double precision :: CF 
  integer :: I, IS, IBL
  
  double precision, parameter  :: CF_THRESHOLD = -1d-5          ! threshold for shear stress bubble detection
  double precision, parameter  :: MIN_BUBBLE_LENGTH = 0.02d0    ! min length of a bubble to be detected
  double precision, parameter  :: DETECT_START = 0.02d0         ! start of detection range 
  double precision, parameter  :: DETECT_END   = 1d0            ! end of detection range

  bubblet%found  = .false.
  bubblet%xstart = 0d0
  bubblet%xend   = 0d0

  bubbleb%found  = .false.
  bubbleb%xstart = 0d0
  bubbleb%xend   = 0d0


  !!Write CF to file together with results
  !open  (unit=iunit, file='CF.txt')
  ! write(*,'(A)') '    X       Y       CF'

! Detect range on upper/lower side where shear stress < 0 

! --- This is the Original stripped down from XFOIL 
  do I=1, N

    if((X(I) >= DETECT_START) .and. (X(I) <= DETECT_END)) then

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
          if     ((CF < CF_THRESHOLD) .and. (bubblet%xend == 0d0)) then
            bubblet%xend = X(I) 
          elseif ((CF < CF_THRESHOLD) .and. (bubblet%xend > 0d0)) then 
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
          if     ((CF < CF_THRESHOLD) .and. (bubbleb%xstart == 0d0)) then
            bubbleb%xstart = X(I) 
          elseif ((CF < CF_THRESHOLD) .and. (bubbleb%xstart > 0d0)) then 
            bubbleb%xend   = X(I) 
          elseif ((CF >= 0d0) .and. (bubbleb%xstart > 0d0)) then 
            bubbleb%xend   = X(I-1) 
            bubbleb%found = .true.
          end if 
        end if  
          ! print *, ' --- bot ------', CL, i, x(i), CF, bubbleb%xstart, bubbleb%xend, bubbleb%found
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

  if (bubblet%found .and. ((bubblet%xend - bubblet%xstart) < MIN_BUBBLE_LENGTH) ) bubblet%found  = .false.
  if (bubbleb%found .and. ((bubbleb%xend - bubbleb%xstart) < MIN_BUBBLE_LENGTH) ) bubbleb%found  = .false.
  
  if (.not. bubblet%found) then
    bubblet%xstart = 0d0
    bubblet%xend = 0d0
  end if 
  if (.not. bubbleb%found) then
    bubbleb%xstart = 0d0
    bubbleb%xend = 0d0
  end if 

  return

end subroutine
  


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

!   allocate(foilout%x(geom_options%npan)
!   allocate(foilout%y(geom_options%npan)
!   do i = 1, geom_options%npan
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
subroutine xfoil_apply_flap_deflection(flap_spec, angle)

  use xfoil_inc
 
  type(flap_spec_type),   intent(in) :: flap_spec

  double precision, intent(in) :: angle
  
  integer y_flap_spec_int

  if (flap_spec%y_flap_spec == 'y/c') then
    y_flap_spec_int = 0
  else
    y_flap_spec_int = 1
  end if

  ! Apply flap deflection

  ! caution: FLAP will change y_flap a little --> ()
  call FLAP((flap_spec%x_flap), (flap_spec%y_flap), y_flap_spec_int, angle)

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

  use xfoil_inc, only : XB, YB, NB
  ! use xfoil_inc, only : SB, XBP, YBP 
  type(airfoil_type), intent(in) :: foil

! Set foil into xfoil buffer foil
  NB = size(foil%x)
  XB(1:NB) = foil%x
  YB(1:NB) = foil%y

  ! # test - not needed anymore? 
  ! CALL SCALC(XB,YB,SB,NB)
  ! CALL SEGSPL(XB,XBP,SB,NB)
  ! CALL SEGSPL(YB,YBP,SB,NB)

! Also copy buffer airfoil to xfoil current foil. This is also made in PANGEN -
!        ... but PANGEN shouldn't always be called before xfoil calculations
  call ABCOPY (.true.)

end subroutine xfoil_set_airfoil



subroutine xfoil_cleanup()

  !------------------------------------------------------------------------------
  !! Deallocates memory in xfoil
  !------------------------------------------------------------------------------

  use xfoil_inc

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



subroutine xfoil_init_BL (show_details)

  !------------------------------------------------------------------------------
  !! Init Boundary layer of xfoil viscous calculation  
  !------------------------------------------------------------------------------

  use xfoil_inc, only : LIPAN, LBLINI

  logical, intent(in) :: show_details

  LIPAN  = .false.
  LBLINI = .false.

  if(show_details) call print_colored (COLOR_NOTE, 'i')

end subroutine xfoil_init_BL 



subroutine xfoil_reload_airfoil(foil)

  !-------------------------------------------------------------------------
  !! Reloads airfoil from xfoil buffer foil
  !-------------------------------------------------------------------------

  use xfoil_inc, only : XB, YB, NB

  type(airfoil_type), intent(inout) :: foil

  if (allocated (foil%x))  deallocate (foil%x)
  if (allocated (foil%y))  deallocate (foil%y)
  allocate(foil%x(NB))
  allocate(foil%y(NB))

  foil%x = XB(1:NB)
  foil%y = YB(1:NB)
  
end subroutine xfoil_reload_airfoil



  subroutine init_outlier_ifNeeded (npoints)

    !------------------------------------------------------------------------------
    !! statistics to handle out lier (flip) detection of drag and lift 
    !------------------------------------------------------------------------------

    integer, intent (in) :: npoints 

    ! if outlier statistics are already available and npoints didn't change - do nothing 
    if (allocated (outlier_stats)) then 
      if (size(outlier_stats) == npoints) then 
        return
      else 
        !$omp critical
        deallocate (outlier_stats)
        !$omp end critical
      end if 
    end if 

    !$omp critical
    allocate (outlier_stats(npoints))
    outlier_stats%nvalue   = 0
    outlier_stats%no_check = .false.
    outlier_stats%minval   = 0.d0
    outlier_stats%maxval   = 0.d0
    outlier_stats%meanval  = 0.d0
    !$omp end critical

  end subroutine init_outlier_ifNeeded



  subroutine update_outlier_stats (iop, new_value)

    !------------------------------------------------------------------------------
    !! update statistics for out lier (flip) detection of drag and lift 
    !------------------------------------------------------------------------------

    doubleprecision, intent (in) :: new_value 
    integer, intent (in)         :: iop
    integer                      :: nmean

    if (.not. allocated (outlier_stats)) return

    !$omp critical (update)
    
    if (outlier_stats(iop)%nvalue == 0) then      ! first value is best we have (could be outlier!)

      outlier_stats(iop)%minval  = new_value 
      outlier_stats(iop)%maxval  = new_value 
      outlier_stats(iop)%meanval = new_value 
      outlier_stats(iop)%nvalue  =  1

    elseif (outlier_stats(iop)%nvalue == 1 .and. &
            new_value < (outlier_stats(iop)%maxval / 2d0)) then ! first value was probably outlier
      outlier_stats(iop)%minval  = new_value 
      outlier_stats(iop)%maxval  = new_value
      outlier_stats(iop)%meanval = new_value            ! reset statistics
      outlier_stats(iop)%nvalue  = 1

    elseif (.not. is_outlier (iop, new_value)) then   ! normal handling
      outlier_stats(iop)%minval  = min (outlier_stats(iop)%minval, new_value) 
      outlier_stats(iop)%maxval  = max (outlier_stats(iop)%maxval, new_value)
      nmean = min (outlier_stats(iop)%nvalue, 50)    ! take max 50 values for mean, so it may develop...
      outlier_stats(iop)%meanval = (outlier_stats(iop)%meanval * nmean + new_value) / (nmean + 1)
      outlier_stats(iop)%nvalue  = outlier_stats(iop)%nvalue + 1
    end if 
    
    !$omp end critical (update)

  end subroutine 



  function is_outlier (iop, check_value) 

    !------------------------------------------------------------------------------
    !! detect out lier (flip) of drag and lift 
    !------------------------------------------------------------------------------

    doubleprecision, intent (in) :: check_value
    integer, intent (in)         :: iop
    logical :: is_outlier 

    doubleprecision ::  dev_from_mean, max_meanval

    is_outlier = .false. 

    if (.not. allocated (outlier_stats)) return

    if(outlier_stats(iop)%nvalue > 0 .and. (.not. outlier_stats(iop)%no_check)) then           !do we have enough values to check? 

      dev_from_mean    = abs(check_value - outlier_stats(iop)%meanval)/max(0.0001d0, outlier_stats(iop)%meanval) 

      if (dev_from_mean > 0.4d0   .or. &                          ! deviation from mean > 40%? 
          check_value   <  (outlier_stats(iop)%minval / 2d0) .or. &  ! half of minval? 
          check_value   >  (outlier_stats(iop)%maxval * 2d0)) then   ! double of maxval? 

        is_outlier = .true.

        !call show_outlier (iop, check_value) 
      
      end if

    ! In case of polar generation we have no values for iop  (every op is unique) 
    !  -> is check_value greater than 8 times (cd) value of other op_points? (empirical)    

    elseif (outlier_stats(iop)%nvalue == 0 ) then        

      max_meanval = maxval (outlier_stats%meanval)
      if (max_meanval > 0d0) then
        if  (check_value > (8d0 * max_meanval)) then 
          is_outlier = .true.
        end if 
      end if 

    end if 

    if (is_outlier) then 
      !$omp atomic 
      stats%noutlier = stats%noutlier + 1
    end if 

  end function 


  subroutine repair_polar_outlier (op_points_spec, op_points_result)

    !----------------------------------------------------------------------------
    !! remove cd outlier in op_points_result by setting to not converged 
    !----------------------------------------------------------------------------

    type(op_point_spec_type), intent(in)                  :: op_points_spec (:)
    type(op_point_result_type), intent(inout)             :: op_points_result (:)

    type(op_point_result_type)  :: op
    integer                     :: i, noppoint, nresult, i_cd_max
    doubleprecision             :: cd_min, cd_max 


    noppoint = size(op_points_spec,1) 
    nresult  = 0 

    ! Sanity checks

    if (noppoint < 5) then                           ! seems to be no polar  
      return
    end if

    ! get cd min/max 

    cd_min = 100d0 

    do i = 1, noppoint
      op = op_points_result(i) 
      if (op%converged) then
        cd_min = min (cd_min, op%cd) 
        nresult = nresult + 1
      end if
    end do 

    if (nresult < 5 .or. cd_min == 0d0) then         ! seems to be no polar  
      return
    end if

    ! repair ops with cd much more than cd_min 

    cd_max = 0d0 
    i_cd_max = 1000

    do i = 1, noppoint
      op = op_points_result(i) 
      if (op%converged) then

        ! cd much more than cd_min 

        if (op%cd > (cd_min * 20d0 )) then

          op_points_result(i)%converged = .false.
          !$omp atomic 
          stats%noutlier = stats%noutlier + 1

        ! find (new) cd_max 
        else if (op%cd > cd_max) then
          cd_max = op%cd 
          i_cd_max = i 
        end if 

      end if
    end do 

    ! repair ops with decreasing cd after cd_max

    do i = 1, noppoint
      op = op_points_result(i) 
      if (op%converged) then

        if (i > i_cd_max .and. (op%cd < cd_max)) then

          op_points_result(i)%converged = .false.
          !$omp atomic 
          stats%noutlier = stats%noutlier + 1

        end if 
      end if
    end do 

  end subroutine
  
  

  subroutine show_outlier (iop, check_value)

    !------------------------------------------------------------------------------
    !! test: show outlier statistics 
    !------------------------------------------------------------------------------

    doubleprecision, intent (in) :: check_value
    integer, intent (in) :: iop

    write (*,'(//,10x, "Outlier detect for op",I2,4x,"cd =", F7.4)', advance='no') iop, check_value
    write (*,'(2x,"Min",F7.4,"  Mean", F7.4, "  Max",F7.4, "   nstat_values ",A)') &
        outlier_stats(iop)%minval, outlier_stats(iop)%meanval, outlier_stats(iop)%maxval, &
        stri(outlier_stats(iop)%nvalue)
    write (*,*)

  end subroutine show_outlier



  function cl_changed (spec_cl, op_point, cl)

    !! Check if lift has changed although it should be fix with spec_cl 

    doubleprecision, intent (in) :: op_point, cl
    logical, intent (in) :: spec_cl
    logical :: cl_changed

    if (spec_cl .and. (abs(cl - op_point) > 0.01d0)) then
      cl_changed = .true.
    else
      cl_changed = .false.
    end if 

  end function cl_changed



  
  subroutine xfoil_stats_print (intent)
   
    !----------------------------------------------------------------------------
    !! print current xfoil violation statistics
    !----------------------------------------------------------------------------
 
    integer, intent(in), optional   :: intent
    ! integer      :: rate, minutes, seconds, 
    integer      :: i

    if (stats%ncalc == 0) return                 ! no calcs up to now ...

    if (present (intent)) then 
      i = intent
    else
      i = 10 
    end if 

    call print_colored (COLOR_PALE, repeat(' ',i)//"Xfoil statistics   : ") 
    
    call print_colored (COLOR_NOTE, stri(stats%ncalc)//" evals")
    if (stats%nretry_ok > 0) &
      call print_colored (COLOR_NOTE, ", "//stri(stats%nretry_ok)//" retries ok")
    if (stats%nretry_failed > 0) &
      call print_colored (COLOR_NOTE, ", "//stri(stats%nretry_failed)//" retries failed")
    if (stats%noutlier > 0) then 
      call print_colored (COLOR_NOTE, ", ")
      call print_colored (COLOR_FEATURE, stri(stats%noutlier)//" outlier")
    end if 

    ! time measured is total thread time - so difficult to read ...

    ! call system_clock(count_rate=rate)

    ! minutes =  stats%it_specal / (rate * 60) 
    ! seconds =  mod ((stats%it_specal / rate), 60) 
    ! call print_colored (COLOR_NOTE, stri(minutes)//":"//stri(seconds)//" in specal, ")

    ! minutes =  stats%it_speccl / (rate * 60) 
    ! seconds =  mod ((stats%it_speccl / rate), 60) 
    ! call print_colored (COLOR_NOTE, stri(minutes)//":"//stri(seconds)//" in speccl, ")

    ! minutes =  stats%it_viscal / (rate * 60) 
    ! seconds =  mod ((stats%it_viscal / rate), 60) 
    ! call print_colored (COLOR_NOTE, stri(minutes)//":"//stri(seconds)//" in viscal")

    print *

  end subroutine 



end module xfoil_driver



