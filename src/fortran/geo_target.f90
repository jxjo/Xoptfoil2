module geo_target

  ! Core base module for geometry target specification and evaluation.

  use os_util,     only : my_stop, Q_GOOD, Q_NO, r_quality
  use print_util,  only : quoted
  use string_util, only : to_lower

  implicit none
  private

  integer, parameter, public :: GEO_TARGET_THICKNESS  = 1
  integer, parameter, public :: GEO_TARGET_CAMBER     = 2
  integer, parameter, public :: GEO_TARGET_MATCH_FOIL = 3

  type geo_target_type
    integer                   :: type           ! enum e.g. GEO_TARGET_THICKNESS
    double precision          :: target_value   ! target value to achieve
    character(:), allocatable :: target_string  ! alt. target argument e.g. name of airfoil
    double precision          :: seed_value     ! the value of the seed airfoil
    double precision          :: reference_value ! to scale improvement (depends on type)

    double precision          :: weighting = 1d0   ! weighting within objective function
    double precision          :: weighting_user = 1d0    ! original weighting entered by user
  end type geo_target_type

  type geo_result_type                           ! results of geometry evaluation
    double precision   :: maxt, xmaxt            ! thickness of airfoil
    double precision   :: maxc, xmaxc            ! camber of airfoil
    double precision   :: top_curv_le, top_curv_te   ! curvature at le and te
    double precision   :: bot_curv_le, bot_curv_te
    double precision   :: match_top_deviation    ! norm2 deviation on top side
    double precision   :: match_bot_deviation    ! norm2 deviation on bot side
  end type geo_result_type

  type geo_target_eval_type
    double precision   :: objective              ! contribution to overall objective function
    double precision   :: target_deviation       ! remaining deviation to target in percent
    double precision   :: target_deviation_abs   ! remaining deviation to target in physical units
    logical            :: target_reached         ! is the target reached?
    integer            :: quality                ! quality rating (Q_GOOD/Q_OK/Q_BAD/Q_PROBLEM/Q_NO)
  end type geo_target_eval_type

  public :: geo_target_type
  public :: geo_result_type
  public :: geo_target_eval_type
  public :: geo_target_type_name
  public :: geo_target_type_enum
  public :: geo_target_value
  public :: geo_target_objective
  public :: geo_target_eval
  public :: geo_target_eval_quality
  public :: init_geo_target_seed_ref

contains

  pure function geo_target_type_name(target_type) result(name)

    integer, intent(in) :: target_type

    character(:), allocatable :: name

    select case (target_type)
      case (GEO_TARGET_THICKNESS)
        name = 'thickness'
      case (GEO_TARGET_CAMBER)
        name = 'camber'
      case (GEO_TARGET_MATCH_FOIL)
        name = 'match-foil'
      case default
        name = 'unknown'
    end select

  end function geo_target_type_name



  pure integer function geo_target_type_enum(name) result(target_type)

    character(*), intent(in) :: name

    select case (to_lower(trim(name)))
      case ('thickness')
        target_type = GEO_TARGET_THICKNESS
      case ('camber')
        target_type = GEO_TARGET_CAMBER
      case ('match-foil')
        target_type = GEO_TARGET_MATCH_FOIL
      case default
        target_type = 0
    end select

  end function geo_target_type_enum



  function geo_target_value(geo_target, geo_result) result(value)

    type(geo_target_type), intent(in) :: geo_target
    type(geo_result_type), intent(in) :: geo_result

    double precision :: value

    select case (geo_target%type)
      case (GEO_TARGET_THICKNESS)
        value = geo_result%maxt
      case (GEO_TARGET_CAMBER)
        value = geo_result%maxc
      case (GEO_TARGET_MATCH_FOIL)
        value = geo_result%match_top_deviation + geo_result%match_bot_deviation
      case default
        call my_stop ("Unknown geo target type: "//quoted(geo_target_type_name(geo_target%type)))
        value = 0d0
    end select

  end function geo_target_value



  function geo_target_objective (geo_result, geo_target) result (obj)

    ! Returns the weighted, dimensionless objective contribution
    ! of a single geo target.

    type(geo_result_type),   intent(in) :: geo_result
    type(geo_target_type),   intent(in) :: geo_target

    double precision :: obj
    double precision :: val, seed_val, target_val, correction, seed_obj

    val = geo_target_value(geo_target, geo_result)
    seed_val = geo_target%seed_value
    target_val = geo_target%target_value

    select case (geo_target%type)
      case (GEO_TARGET_THICKNESS)
        correction = 1.2d0                          ! thickness is less sensitive to changes
      case (GEO_TARGET_CAMBER)
        correction = 0.7d0                          ! camber is more sensitive to changes
      case (GEO_TARGET_MATCH_FOIL)
        correction = 1d0
      case default
        call my_stop ("Unknown geo target type: "//quoted(geo_target_type_name(geo_target%type)))
        correction = 1d0
    end select

    obj = geo_target%reference_value + abs(target_val - val) * correction
    seed_obj = geo_target%reference_value + abs(target_val - seed_val) * correction

    if (seed_obj <= 1d-12) then
      if (obj <= 1d-12) then
        obj = 1d0
      else
        obj = obj / 1d-12
      end if
    else
      obj = obj / seed_obj
    end if

    obj = obj * geo_target%weighting

  end function geo_target_objective



  function geo_target_eval(geo_spec, geo_result) result(eval)

    type(geo_target_type), intent(in) :: geo_spec
    type(geo_result_type), intent(in) :: geo_result

    type(geo_target_eval_type) :: eval
    double precision           :: dist, scale_abs

    eval%objective            = 0d0
    eval%target_deviation     = 0d0
    eval%target_deviation_abs = 0d0
    eval%target_reached       = .false.
    eval%quality              = Q_NO

    eval%objective = geo_target_objective(geo_result, geo_spec) 

    select case (geo_spec%type)
      case (GEO_TARGET_THICKNESS, GEO_TARGET_CAMBER)
        dist = geo_target_value(geo_spec, geo_result) - geo_spec%target_value
        scale_abs = max(abs(geo_spec%target_value), 1d-6)

        ! Keep the signed physical gap for output, but use absolute percent
        ! deviation for quality and progress reporting.
        eval%target_deviation_abs = dist
        eval%target_deviation = abs(dist) / scale_abs * 100d0
        eval%quality = geo_target_eval_quality(geo_spec, eval)
        eval%target_reached = eval%quality == Q_GOOD

      case default
        eval%quality = Q_NO
    end select

  end function geo_target_eval



  integer function geo_target_eval_quality(geo_spec, eval) result(quality)

    type(geo_target_type), intent(in)      :: geo_spec
    type(geo_target_eval_type), intent(in) :: eval

    select case (geo_spec%type)
      case (GEO_TARGET_THICKNESS, GEO_TARGET_CAMBER)
        quality = r_quality(abs(eval%target_deviation), 0.07d0, 2d0, 10d0)
      case default
        quality = Q_NO
    end select

  end function geo_target_eval_quality



  subroutine init_geo_target_seed_ref(geo_spec, geo_result)

    type(geo_target_type), intent(inout) :: geo_spec
    type(geo_result_type), intent(in)    :: geo_result

    geo_spec%seed_value = geo_target_value(geo_spec, geo_result)

    select case (geo_spec%type)
      case (GEO_TARGET_THICKNESS, GEO_TARGET_CAMBER)
        geo_spec%reference_value = geo_spec%seed_value
      case (GEO_TARGET_MATCH_FOIL)
        geo_spec%reference_value = 0d0
      case default
        call my_stop("Unknown geo target_type '"//geo_target_type_name(geo_spec%type)//"'")
    end select

  end subroutine init_geo_target_seed_ref


end module