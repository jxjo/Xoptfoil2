! MIT License
! Copyright (c) 2025 Jochen Guenzel 

module shape_curve

  !-------------------------------------------------------------------------
  ! Generic curve infrastructure for Bezier and B-spline shapes
  ! Provides unified interfaces to reduce code duplication
  !
  ! Generic interfaces allow using the same function names for both curve types:
  !   - curve_eval_y_on_x  (bezier_eval_y_on_x / bspline_eval_y_on_x / polymorphic)
  !   - curve_eval_side    (bezier_eval_side / bspline_eval_side / polymorphic)
  !   - curve_le_curvature (bezier_le_curvature / bspline_le_curvature / polymorphic)
  !   - curve_curvature    (bezier_curvature / bspline_curvature / polymorphic)
  !   - map_dv_to_control_points (map_dv_to_bezier / map_dv_to_bspline / polymorphic)
  !
  ! Compiler selects based on type:
  !   - Concrete types (bezier_spec_type/bspline_spec_type) → compile-time dispatch
  !   - Polymorphic (class(curve_spec_type)) → runtime dispatch via internal select type
  !-------------------------------------------------------------------------

  use math_util,     only : curve_spec_type

  use shape_bezier,  only : bezier_spec_type
  use shape_bezier,  only : bezier_eval_y_on_x, bezier_eval_side
  use shape_bezier,  only : bezier_le_curvature, bezier_curvature
  use shape_bezier,  only : map_dv_to_bezier, bezier_get_dv0

  use shape_bspline, only : bspline_spec_type
  use shape_bspline, only : bspline_eval_y_on_x, bspline_eval_side
  use shape_bspline, only : bspline_le_curvature, bspline_curvature
  use shape_bspline, only : map_dv_to_bspline, bspline_get_dv0

  implicit none
  private

  ! Curve type constants
  character(*), parameter, public :: CURVE_TYPE_BEZIER = 'bezier'
  character(*), parameter, public :: CURVE_TYPE_BSPLINE = 'bspline'

  ! ============================================================================
  ! Generic interfaces - work for both bezier_spec_type and bspline_spec_type
  ! ============================================================================

  public :: curve_eval_y_on_x
  interface curve_eval_y_on_x
    module procedure bezier_eval_y_on_x
    module procedure bspline_eval_y_on_x
    module procedure curve_eval_y_on_x_poly     ! polymorphic dispatcher
  end interface

  public :: curve_eval_side
  interface curve_eval_side
    module procedure bezier_eval_side
    module procedure bspline_eval_side
    module procedure curve_eval_side_poly       ! polymorphic dispatcher
  end interface

  public :: curve_le_curvature
  interface curve_le_curvature
    module procedure bezier_le_curvature
    module procedure bspline_le_curvature
    module procedure curve_le_curvature_poly    ! polymorphic dispatcher
  end interface

  public :: curve_curvature
  interface curve_curvature
    module procedure bezier_curvature
    module procedure bspline_curvature
    module procedure curve_curvature_poly       ! polymorphic dispatcher
  end interface

  public :: map_dv_to_control_points
  interface map_dv_to_control_points
    module procedure map_dv_to_bezier
    module procedure map_dv_to_bspline
    module procedure map_dv_to_control_points_poly  ! polymorphic dispatcher
  end interface

  public :: curve_get_dv0
  interface curve_get_dv0
    module procedure bezier_get_dv0
    module procedure bspline_get_dv0
  end interface

contains

  ! ==========================================================================
  ! Polymorphic dispatcher functions
  ! Handle runtime type selection for class(curve_spec_type) arguments
  ! ==========================================================================

  function curve_eval_y_on_x_poly (curve, x, epsilon) result(y)
    !! Polymorphic dispatcher for curve_eval_y_on_x
    !! Handles class(curve_spec_type) via internal select type
    class(curve_spec_type), intent(in) :: curve 
    double precision, intent(in)       :: x
    double precision, optional, intent(in) :: epsilon
    double precision :: y

    select type (curve)
      type is (bezier_spec_type)
        y = bezier_eval_y_on_x(curve, x, epsilon)
      type is (bspline_spec_type)
        y = bspline_eval_y_on_x(curve, x, epsilon)
    end select
  end function

  subroutine curve_eval_side_poly (curve, npoint_side, x_side, y_side, curvature_side, use_arc_length)
    !! Polymorphic dispatcher for curve_eval_side
    !! Handles class(curve_spec_type) via internal select type
    class(curve_spec_type), intent(in)     :: curve
    integer, intent(in)                     :: npoint_side
    double precision, allocatable, intent(out) :: x_side(:), y_side(:)
    double precision, allocatable, intent(out), optional :: curvature_side(:)
    logical, intent(in), optional           :: use_arc_length

    select type (curve)
      type is (bezier_spec_type)
        call bezier_eval_side(curve, npoint_side, x_side, y_side, curvature_side, use_arc_length)
      type is (bspline_spec_type)
        call bspline_eval_side(curve, npoint_side, x_side, y_side, curvature_side, use_arc_length)
    end select
  end subroutine

  function curve_le_curvature_poly (curve) result(curv)
    !! Polymorphic dispatcher for curve_le_curvature
    !! Handles class(curve_spec_type) via internal select type
    class(curve_spec_type), intent(in) :: curve
    double precision :: curv

    select type (curve)
      type is (bezier_spec_type)
        curv = bezier_le_curvature(curve)
      type is (bspline_spec_type)
        curv = bspline_le_curvature(curve)
    end select
  end function

  function curve_curvature_poly (curve, u) result(curv)
    !! Polymorphic dispatcher for curve_curvature
    !! Handles class(curve_spec_type) via internal select type
    class(curve_spec_type), intent(in) :: curve
    double precision, intent(in) :: u
    double precision :: curv

    select type (curve)
      type is (bezier_spec_type)
        curv = bezier_curvature(curve, u)
      type is (bspline_spec_type)
        curv = bspline_curvature(curve, u)
    end select
  end function

  subroutine map_dv_to_control_points_poly (is_bot, dv_in, te_gap, curve, le_curv)
    !! Polymorphic dispatcher for map_dv_to_control_points
    !! Handles class(curve_spec_type) via internal select type
    logical, intent(in)                  :: is_bot
    double precision, intent(in)         :: dv_in(:)
    double precision, intent(in)         :: te_gap
    class(curve_spec_type), intent(out)  :: curve
    double precision, optional, intent(in) :: le_curv

    select type (curve)
      type is (bezier_spec_type)
        call map_dv_to_bezier(is_bot, dv_in, te_gap, curve, le_curv)
      type is (bspline_spec_type)
        call map_dv_to_bspline(is_bot, dv_in, te_gap, curve, le_curv)
    end select
  end subroutine

end module shape_curve
