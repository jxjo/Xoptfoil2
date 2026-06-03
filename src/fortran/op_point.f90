module op_point

  ! Core base module for operating point specification and evaluation.

  use commons       
  use os_util
  use print_util, only : print_fixed
  use string_util

  implicit none
  private

  type re_type 
    double precision            :: number                 ! Reynolds Number
    integer                     :: type                   ! Type 1 or 2 (fixed lift)
  end type re_type

  ! result of xfoil boundary layer (BL) infos of an op_pooint

  type bubble_type      
    logical                     :: found = .false.        ! a bubble was detected           
    double precision            :: xstart = 0d0           ! start of separation: CF (shear stress) < 0
    double precision            :: xend = 0d0             ! end   of separation: CF (shear stress) > 0
  end type bubble_type                              

  ! the stored seed reference for one op_point, to track improvement during optimization

  type :: seed_ref_type
    logical                     :: valid     = .false.
    double precision            :: value     = 0d0        ! physical value of the op_point goal for seed
    double precision            :: miss      = 0d0        ! deviation from target value
    double precision            :: objective = 0d0        ! objective function value
  end type seed_ref_type

  ! defines an op_point for xfoil calculation
 
  type op_point_spec_type  
  
    ! aero - xfoil 
    logical                     :: spec_cl                 ! op based on alpha or cl
    double precision            :: value                   ! base value of cl or alpha
    type (re_type)              :: re, ma                  ! Reynolds and Mach 
    double precision            :: ncrit                   ! xfoil ncrit

    ! objective function 
    character(:), allocatable   :: optimization_type       ! eg 'min-drag'
    integer                     :: opt_type                ! enum for optimization type - min, max, target
    double precision            :: target_value            ! target value to achieve
    logical                     :: allow_improved_target = .true. ! the result value may be better than target value

    ! flap setting
    double precision            :: flap_angle              ! fix or initial  flap angle of this op op point 
    logical                     :: flap_optimize = .false. ! optimize flap angle

    ! seed reference for tracking improvement during optimization
    type(seed_ref_type)         :: seed

    ! weighting 
    double precision            :: weighting = 1d0         ! weighting within objective function
    double precision            :: weighting_user = 1d0    ! original weighting entered by user

  end type op_point_spec_type

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

  ! Hold results of evaltion of an op_point, objective contribution and improvement info

  type op_point_eval_type
    double precision            :: objective                ! contribution to overall objective function 
    double precision            :: improvement              ! improvement vs seed reference in percent
    double precision            :: improvement_abs          ! signed absolute improvement vs seed reference
    logical                     :: improvement_uses_base    ! .true. if improvement percent uses a base value
    double precision            :: target_deviation         ! remaining deviation to target % (if opt_type is target)
    double precision            :: target_deviation_abs     ! remaining deviation to target in physical units (if opt_type is target)
    logical                     :: target_reached           ! is the target reached ? (if opt_type is target)
    integer                     :: quality                  ! quality rating (Q_GOOD/Q_OK/Q_BAD/Q_PROBLEM/Q_NO)
  end type op_point_eval_type


  ! Public constants

  integer, parameter, public :: OPT_MIN_CD       = 1
  integer, parameter, public :: OPT_MAX_CL       = 2
  integer, parameter, public :: OPT_MAX_GLIDE    = 3
  integer, parameter, public :: OPT_MIN_SINK     = 4
  integer, parameter, public :: OPT_MAX_XTR      = 5
  integer, parameter, public :: OPT_TARGET_CD    = 6
  integer, parameter, public :: OPT_TARGET_CL    = 7
  integer, parameter, public :: OPT_TARGET_GLIDE = 8
  integer, parameter, public :: OPT_TARGET_CM    = 9

  integer, parameter, public :: OPT_TYPES_ALL(9) = [ &
    OPT_MIN_CD, OPT_MAX_CL, OPT_MAX_GLIDE, OPT_MIN_SINK, OPT_MAX_XTR, &
    OPT_TARGET_CD, OPT_TARGET_CL, OPT_TARGET_GLIDE, OPT_TARGET_CM ]

  public :: re_type
  public :: bubble_type
  public :: seed_ref_type
  public :: op_point_spec_type 
  public :: op_point_result_type
  public :: op_point_eval_type

  public :: opt_type_name
  public :: opt_type_enum
  public :: opt_type_strength
  public :: is_target
  public :: op_point_value
  public :: op_point_miss
  public :: op_point_objective
  public :: init_op_point_seed_ref
  public :: op_point_eval
  public :: op_point_eval_quality
  public :: print_op_point_objective
  public :: print_op_point_spec
  public :: print_op_point_result


contains

  pure logical function is_target (op_spec) result(it_is)

    ! Returns .true. if goal_type is one of the target goals.

    type(op_point_spec_type), intent(in) :: op_spec

    select case (op_spec%opt_type)
      case (OPT_TARGET_CD, OPT_TARGET_CL, OPT_TARGET_GLIDE, OPT_TARGET_CM)
          it_is = .true.
      case default
          it_is = .false.
    end select

  end function is_target



  pure function opt_type_name(opt_type) result(name)

    integer, intent(in) :: opt_type

    character(:), allocatable :: name

    select case (opt_type)
      case (OPT_MIN_CD)
        name = 'min-drag'
      case (OPT_MAX_CL)
        name = 'max-lift'
      case (OPT_MAX_GLIDE)
        name = 'max-glide'
      case (OPT_MIN_SINK)
        name = 'min-sink'
      case (OPT_MAX_XTR)
        name = 'max-xtr'
      case (OPT_TARGET_CD)
        name = 'target-drag'
      case (OPT_TARGET_CL)
        name = 'target-lift'
      case (OPT_TARGET_GLIDE)
        name = 'target-glide'
      case (OPT_TARGET_CM)
        name = 'target-moment'
      case default
        name = 'unknown'
    end select

  end function opt_type_name



  pure integer function opt_type_enum(name) result(opt_type)

    character(*), intent(in) :: name

    select case (to_lower(trim(name)))
      case ('min-drag')
        opt_type = OPT_MIN_CD
      case ('max-lift')
        opt_type = OPT_MAX_CL
      case ('max-glide')
        opt_type = OPT_MAX_GLIDE
      case ('min-sink')
        opt_type = OPT_MIN_SINK
      case ('max-xtr')
        opt_type = OPT_MAX_XTR
      case ('target-drag')
        opt_type = OPT_TARGET_CD
      case ('target-lift')
        opt_type = OPT_TARGET_CL
      case ('target-glide')
        opt_type = OPT_TARGET_GLIDE
      case ('target-moment')
        opt_type = OPT_TARGET_CM
      case default
        opt_type = 0
    end select

  end function opt_type_enum



  pure function opt_type_strength (opt_type) result(strength)

    ! Returns the internal strength for a opt_type.
    !   which controls how strong a change in value affects the objective function.
    ! This is not user input; it is fixed by opt_type semantics.

    integer, intent(in) :: opt_type

    double precision :: strength

    select case (opt_type)
      case (OPT_TARGET_GLIDE)
          strength = 1.1d0
      case default
          strength = 1.0d0
    end select

  end function opt_type_strength



  function op_point_value (op_spec, op) result (val)

    ! Returns the physical value associated with the goal:
    !   cd, cl, cm, or glide = cl/cd.

    type(op_point_spec_type),     intent(in) :: op_spec
    type(op_point_result_type),   intent(in) :: op

    double precision :: val

    val = NOT_DEF_D

    if (.not. op%converged) return

    select case (op_spec%opt_type)

    case (OPT_MIN_CD, OPT_TARGET_CD)
        val = op%cd

      case (OPT_MAX_CL, OPT_TARGET_CL)
        val = op%cl

      case (OPT_TARGET_CM)
        val = op%cm

      case (OPT_MAX_GLIDE, OPT_TARGET_GLIDE)
        if (op%cd > 0.0) &
          val = op%cl / op%cd

      case (OPT_MIN_SINK)
        if (op%cd > 0.0) &
          val = op%cl**2 / op%cd**3

      case (OPT_MAX_XTR)
        val = (op%xtrt + op%xtrb) / 2.0d0

    end select

    if (val == NOT_DEF_D) &
      call my_stop ("Unexpected opt_type in op_point_value: " // stri(op_spec%opt_type))

  end function op_point_value



  function op_point_miss(op_spec, op) result(miss)

    ! Returns the physical target miss for target goals.
    ! For non-target goals, 0d0 is returned.
    !
    ! Symmetric target:
    !   miss = abs(value - target)
    !
    ! One-sided target:
    !   lower-better  -> value - target
    !   higher-better -> target - value
    !
    ! A negative miss means the result is better than the target.

    type(op_point_spec_type),     intent(in) :: op_spec
    type(op_point_result_type),   intent(in) :: op

    double precision  :: val, miss, target_val

    miss = 0d0

    if (.not. is_target(op_spec)) return

    val        = op_point_value (op_spec, op)
    target_val = op_spec%target_value

    if (.not. op_spec%allow_improved_target) then
        miss = abs(val - target_val)
        return
    end if

    if (op_spec%opt_type == OPT_TARGET_CD) then

      miss = val - target_val            ! lower is better

    else
      miss = target_val - val            ! higher is better

    end if

  end function op_point_miss



  function op_point_objective (op_spec, op) result (obj)

    ! Returns the weighted, dimensionless objective contribution
    ! of a single op_point.
    !
    ! Absolute goals:
    !   min_cd    ->  (value / seed_value) ** strength
    !   max_cl    ->  (seed_value / value) ** strength
    !   max_glide ->  (seed_value / value) ** strength
    !
    ! Target goals reuse op_point_miss semantics to switch
    ! between one-sided and symmetric target handling.

    type(op_point_spec_type),     intent(in) :: op_spec
    type(op_point_result_type),   intent(in) :: op

    double precision    :: obj, base_val, seed_abs
    double precision    :: val, target_miss, strength, seed_val, target_val

    obj = 0d0

    if (.not. op%converged) return
    if (.not. op_spec%seed%valid) then
      call my_stop ("Seed reference not valid for op point ")
    end if

    val        = op_point_value(op_spec, op)
    seed_val   = op_spec%seed%value
    seed_abs   = max(abs(seed_val), 1d-6)                  ! avoid division by zero
    target_val = op_spec%target_value
    target_miss = op_point_miss(op_spec, op)

    if (op_spec%allow_improved_target) then
      target_miss = max(target_miss, 0d0)
    end if

    select case (op_spec%opt_type)

      ! For absolute goals, the objective is based on the physical value and seed reference.

      case (OPT_MIN_CD)

        obj = (val / seed_abs)

      case (OPT_TARGET_CD)

        obj = (target_val + target_miss) / seed_abs

      case (OPT_MAX_GLIDE, OPT_MAX_XTR, OPT_MIN_SINK)

        obj = (seed_abs / val) 

      case (OPT_TARGET_GLIDE)

        obj = seed_abs / (target_val - target_miss)

      ! values can be zero - we need a base value 

      case (OPT_MAX_CL)

        base_val = 1d0
        obj = (base_val + seed_val) / (base_val + val)

      case (OPT_TARGET_CL)

        base_val = 1d0
        obj = (base_val + seed_val) / (base_val + target_val - target_miss)

      case (OPT_TARGET_CM)

        base_val = 0.1d0
        obj = max(base_val + seed_val, 1d-6) / max(base_val + target_val - target_miss, 1d-6)

      case default

        obj = 1d0 

    end select

    ! apply strength to amplify the effect of changes in value on the objective function

    strength   = opt_type_strength(op_spec%opt_type)
    
    obj = obj ** strength

    if (is_target(op_spec)) then
      if (target_miss <= 0d0) obj = min(obj, 1d0)
    end if

    obj = obj * op_spec%weighting


  end function op_point_objective



  subroutine init_op_point_seed_ref (op_spec, op_seed)

    ! Initializes the stored seed reference for one op_point.
    ! This should be called once before optimization starts.

    type(op_point_spec_type),   intent(inout) :: op_spec
    type(op_point_result_type), intent(in)    :: op_seed

    op_spec%seed%valid     = .false.
    op_spec%seed%value     = 0d0
    op_spec%seed%miss      = 0d0

    if (.not. op_seed%converged) return

    op_spec%seed%value     = op_point_value     (op_spec, op_seed)

    if (is_target(op_spec)) then
        op_spec%seed%miss = op_point_miss (op_spec, op_seed)
    else
        op_spec%seed%miss = 0.0
    end if

    op_spec%seed%valid = .true.

  end subroutine init_op_point_seed_ref



  subroutine op_point_improvement_stats (op_spec, op, delta, reference_abs, uses_base)

    ! Returns the signed absolute improvement delta relative to seed and the
    ! denominator for the improvement percentage.
    !
    ! The percentage is normally based on the stored seed reference magnitude.
    ! For CL- and CM-based goals, a fixed base value is used when the seed
    ! reference is too close to zero, so improvement can still be reported as
    ! a percent rather than falling back to an absolute-only mode.

    type(op_point_spec_type),   intent(in)  :: op_spec
    type(op_point_result_type), intent(in)  :: op
    double precision,           intent(out) :: delta
    double precision,           intent(out) :: reference_abs
    logical,                    intent(out) :: uses_base

    double precision :: current_val, reference_val, scale_abs
    logical          :: lower_is_better

    if (is_target(op_spec)) then
      reference_val   = op_spec%seed%miss
      current_val     = op_point_miss(op_spec, op)
      scale_abs       = abs(op_spec%seed%value)
      if (op_spec%opt_type == OPT_TARGET_CM) scale_abs = abs(op_spec%seed%miss)
      lower_is_better = .true.
    else
      reference_val = op_spec%seed%value
      current_val   = op_point_value(op_spec, op)
      scale_abs     = abs(reference_val)

      select case (op_spec%opt_type)
        case (OPT_MIN_CD)
          lower_is_better = .true.
        case default
          lower_is_better = .false.
      end select
    end if

    if (lower_is_better) then
      delta = reference_val - current_val
    else
      delta = current_val - reference_val
    end if

    uses_base     = .false.
    reference_abs = scale_abs

    select case (op_spec%opt_type)
      case (OPT_MAX_CL, OPT_TARGET_CL)
        if (reference_abs < 0.1d0) then
          reference_abs = 1d0
          uses_base = .true.
        end if
      case (OPT_TARGET_CM)
        if (reference_abs < 0.01d0) then
          reference_abs = 0.1d0
          uses_base = .true.
        end if
    end select

    reference_abs = max(reference_abs, 1d-6)

  end subroutine op_point_improvement_stats


  integer function op_point_eval_quality (op_spec, eval) result(quality)

    ! Returns the reporting quality classification for an evaluated op_point.

    type(op_point_spec_type), intent(in) :: op_spec
    type(op_point_eval_type), intent(in) :: eval

    if (is_target(op_spec)) then
      quality = r_quality (eval%target_deviation, 0.1d0, 2d0, 10d0)
    else if (eval%improvement <= 0d0) then
      quality = Q_BAD
    else if (eval%improvement < 2d0) then
      quality = Q_OK
    else
      quality = Q_GOOD
    end if

  end function op_point_eval_quality


  
  function op_point_eval (op_spec, op) result (eval)

    ! Evaluates the op_point objective contribution and reporting info.
    !   - improvement is always reported in percent
    !   - improvement_abs is always the signed absolute delta to the seed
    !   - improvement_uses_base shows whether the percent uses a fixed base
    !     value instead of the stored seed reference magnitude
    !   - target_deviation is the remaining gap to target, so it is never
    !     negative and becomes zero once a one-sided target is reached

    type(op_point_spec_type),   intent(in) :: op_spec
    type(op_point_result_type), intent(in) :: op

    type(op_point_eval_type) :: eval

    double precision :: delta, reference_abs, target_scale_abs, target_miss
    logical          :: uses_base

    eval%objective            = 0d0
    eval%improvement          = 0d0
    eval%improvement_abs      = 0d0
    eval%improvement_uses_base = .false.
    eval%target_deviation     = 0d0
    eval%target_deviation_abs = 0d0
    eval%target_reached       = .false.
    eval%quality              = Q_NO

    if (.not. op%converged) return

    eval%objective = op_point_objective(op_spec, op) 

    call op_point_improvement_stats(op_spec, op, delta, reference_abs, uses_base)
    eval%improvement_uses_base = uses_base
    eval%improvement_abs = delta
    eval%improvement = delta / reference_abs * 100d0

    if (.not. is_target(op_spec)) then
      eval%quality = op_point_eval_quality(op_spec, eval)
      return
    end if

    target_miss = op_point_miss(op_spec, op)

    if (op_spec%allow_improved_target) then
      eval%target_deviation_abs = max(target_miss, 0d0)
    else
      eval%target_deviation_abs = abs(target_miss)
    end if
    eval%target_reached = eval%target_deviation_abs <= 0d0

    select case (op_spec%opt_type)
      case (OPT_TARGET_CM)
        target_scale_abs = max(abs(0.1d0 + op_spec%target_value), 1d-6)
      case default
        target_scale_abs = max(abs(op_spec%target_value), 1d-6)
    end select

    eval%target_deviation = eval%target_deviation_abs / target_scale_abs * 100d0
    eval%quality = op_point_eval_quality(op_spec, eval)


  end function op_point_eval



  subroutine print_op_point_objective (op_spec, op, with_header, indent)

    type(op_point_spec_type), intent(in) :: op_spec
    type(op_point_result_type), intent(in) :: op
    logical, intent(in), optional        :: with_header
    integer, intent(in), optional        :: indent

    logical                   :: show_header
    integer                   :: i_indent
    character(5)              :: improvement_kind
    character(:), allocatable :: opt_name
    type(op_point_eval_type)  :: op_eval
    double precision          :: strength, objective, improvement, current_val, target_val

    show_header = .false.
    if (present(with_header)) show_header = with_header
    i_indent = 0
    if (present(indent)) i_indent = max(0, min(indent, 80))

    opt_name    = opt_type_name(op_spec%opt_type)
    strength    = opt_type_strength(op_spec%opt_type)
    objective   = op_point_objective(op_spec, op)
    op_eval     = op_point_eval(op_spec, op)

    improvement = op_eval%improvement
    if (op_eval%improvement_uses_base) then
      improvement_kind = 'base%'
    else
      improvement_kind = 'seed%'
    end if

    current_val = op_point_value(op_spec, op)
    target_val  = op_spec%target_value

    if (show_header) then
      if (i_indent > 0) call print_colored (COLOR_NORMAL, repeat(' ', i_indent))
      call print_fixed (' type',       16)
      call print_fixed ('seed_val',     9, adjust_right=.true.)
      call print_fixed ('seed_miss',   10, adjust_right=.true.)
      call print_fixed ('value',       10, adjust_right=.true.)
      call print_fixed ('target',      10, adjust_right=.true.)
      call print_fixed ('strength',    10, adjust_right=.true.)
      call print_fixed ('objective',   12, adjust_right=.true.)
      call print_fixed ('improv',      10, adjust_right=.true.)
      call print_fixed ('kind',         6, adjust_right=.true.)
      print *

      if (i_indent > 0) call print_colored (COLOR_NORMAL, repeat(' ', i_indent))
      call print_colored (COLOR_PALE, repeat('-',16))
      call print_colored (COLOR_PALE, repeat('-', 9))
      call print_colored (COLOR_PALE, repeat('-',10))
      call print_colored (COLOR_PALE, repeat('-',10))
      call print_colored (COLOR_PALE, repeat('-',10))
      call print_colored (COLOR_PALE, repeat('-',10))
      call print_colored (COLOR_PALE, repeat('-',12))
      call print_colored (COLOR_PALE, repeat('-',10))
      call print_colored (COLOR_PALE, repeat('-', 6))
      print *
    end if

    if (i_indent > 0) call print_colored (COLOR_NORMAL, repeat(' ', i_indent))

    call print_fixed (' ' // opt_name, 16)
    call print_colored_f ( 9, '(F9.4)',   -1, op_spec%seed%value)
    call print_colored_f (10, '(F10.4)',  -1, op_spec%seed%miss)
    call print_colored_f (10, '(F10.4)',  -1, current_val)
    if (is_target(op_spec)) then
      call print_colored_f (10, '(F10.4)',  -1, target_val)
    else
      call print_fixed ('', 10, adjust_right=.true.)
    end if
    call print_colored_f (10, '(F10.4)',  -1, strength)
    call print_colored_f (12, '(F12.5)',  -1, objective)
    call print_colored_f (10, '(F10.2)',  -1, improvement)
    call print_fixed (improvement_kind, 6, adjust_right=.true.)
    print *

  end subroutine print_op_point_objective



  subroutine print_op_point_spec (op_spec, with_header, indent)

    type(op_point_spec_type), intent(in) :: op_spec
    logical, intent(in), optional        :: with_header
    integer, intent(in), optional        :: indent

    logical                   :: show_header
    integer                   :: i_indent, reynolds_k
    character(5)              :: mode
    character(:), allocatable :: opt_name
    character(:), allocatable :: reynolds_label

    show_header = .false.
    if (present(with_header)) show_header = with_header
    i_indent = 0
    if (present(indent)) i_indent = max(0, min(indent, 80))

    if (op_spec%spec_cl) then
      mode = 'cl  '
    else
      mode = 'alpha'
    end if

    opt_name = opt_type_name(op_spec%opt_type)
    reynolds_k = nint(op_spec%re%number / 1000d0)
    reynolds_label = stri(reynolds_k) // 'k'

    if (show_header) then
      if (i_indent > 0) call print_colored (COLOR_NORMAL, repeat(' ', i_indent))
      call print_fixed ('mode',   6)
      call print_fixed ('value', 7,  adjust_right=.true.)
      call print_fixed (' type', 15, adjust_right=.true.)
      call print_fixed ('target',9,  adjust_right=.true.)
      call print_fixed ('Re',     8, adjust_right=.true.)
      call print_fixed ('Ma',     7, adjust_right=.true.)
      call print_fixed ('ncrit',  9, adjust_right=.true.)
      call print_fixed ('flap',   9, adjust_right=.true.)
      print *

      if (i_indent > 0) call print_colored (COLOR_NORMAL, repeat(' ', i_indent))
      call print_colored (COLOR_PALE, repeat('-', 6))
      call print_colored (COLOR_PALE, repeat('-',7))
      call print_colored (COLOR_PALE, repeat('-',15))
      call print_colored (COLOR_PALE, repeat('-',9))
      call print_colored (COLOR_PALE, repeat('-', 8))
      call print_colored (COLOR_PALE, repeat('-', 7))
      call print_colored (COLOR_PALE, repeat('-', 9))
      call print_colored (COLOR_PALE, repeat('-', 9))
      print *
    end if

    if (i_indent > 0) call print_colored (COLOR_NORMAL, repeat(' ', i_indent))
    call print_fixed (mode, 6)
    call print_colored_f (7, '(F7.4)',  -1, op_spec%value)
    call print_fixed (' ' // opt_name, 15, adjust_right=.true.)
    call print_colored_f (9, '(F7.4)',  -1, op_spec%target_value)
    call print_fixed (reynolds_label, 8, adjust_right=.true.)
    call print_colored_f ( 7, '(F7.1)',   -1, op_spec%ma%number)
    call print_colored_f ( 9, '(F9.2)',   -1, op_spec%ncrit)
    call print_colored_f ( 9, '(F9.2)',   -1, op_spec%flap_angle)
    print *

  end subroutine print_op_point_spec



  subroutine print_op_point_result (op, with_header, indent)

    type(op_point_result_type), intent(in) :: op
    logical, intent(in), optional          :: with_header
    integer, intent(in), optional          :: indent

    logical :: show_header
    integer :: i_indent

    show_header = .false.
    if (present(with_header)) show_header = with_header
    i_indent = 0
    if (present(indent)) i_indent = max(0, min(indent, 80))

    if (show_header) then
      if (i_indent > 0) call print_colored (COLOR_NORMAL, repeat(' ', i_indent))
      call print_colored (COLOR_PALE, 'state ')
      call print_fixed ('alpha', 8, adjust_right=.true.)
      call print_fixed ('cl',    9, adjust_right=.true.)
      call print_fixed ('cd',   10, adjust_right=.true.)
      call print_fixed ('cdp',  10, adjust_right=.true.)
      call print_fixed ('cm',    9, adjust_right=.true.)
      call print_fixed ('xtrt',  8, adjust_right=.true.)
      call print_fixed ('xtrb',  8, adjust_right=.true.)
      print *

      if (i_indent > 0) call print_colored (COLOR_NORMAL, repeat(' ', i_indent))
      call print_colored (COLOR_PALE, '----- ')
      call print_colored (COLOR_PALE, repeat('-', 8))
      call print_colored (COLOR_PALE, repeat('-', 9))
      call print_colored (COLOR_PALE, repeat('-',10))
      call print_colored (COLOR_PALE, repeat('-',10))
      call print_colored (COLOR_PALE, repeat('-', 9))
      call print_colored (COLOR_PALE, repeat('-', 8))
      call print_colored (COLOR_PALE, repeat('-', 8))
      print *
    end if

    if (i_indent > 0) call print_colored (COLOR_NORMAL, repeat(' ', i_indent))
    if (op%converged) then
      call print_colored (COLOR_GOOD, 'ok    ')
    else
      call print_colored (COLOR_WARNING, 'fail  ')
    end if

    call print_colored_f ( 8, '(F8.3)',  -1, op%alpha)
    call print_colored_f ( 9, '(F9.4)',  -1, op%cl)
    call print_colored_f (10, '(F10.5)', -1, op%cd)
    call print_colored_f (10, '(F10.5)', -1, op%cdp)
    call print_colored_f ( 9, '(F9.4)',  -1, op%cm)
    call print_colored_f ( 8, '(F8.4)',  -1, op%xtrt)
    call print_colored_f ( 8, '(F8.4)',  -1, op%xtrb)
    print *

  end subroutine print_op_point_result

end module op_point

