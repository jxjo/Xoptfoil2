! MIT License
! Copyright (c) 2026 Jochen Guenzel

module test_op_point

  use op_point
  use os_util,       only : Q_GOOD, Q_BAD, Q_PROBLEM
  use test_util
  use airfoil_base,  only : airfoil_type, create_bezier_JX_GS3_100
  use xfoil_driver,  only : run_op_points_no_flap, xfoil_cleanup, xfoil_init, xfoil_options_type
  use xfoil_driver,  only : xfoil_stats_print

  implicit none

  integer, parameter :: NOPT = size(OPT_TYPES_ALL)

  type(op_point_spec_type), allocatable     :: op_point_specs(:)
  type(op_point_result_type), allocatable   :: op_point_results(:)

contains

  subroutine test_op_point_all ()
    call test_op_point_specs()
    call test_op_point_results()
    call test_op_point_objective()
  end subroutine

  subroutine ensure_op_point_specs ()

    integer :: i
    double precision :: cl_value

    if (allocated(op_point_specs)) return

    allocate(op_point_specs(NOPT))

    call print_action ("Create op point specs ...")

    cl_value = 0.0d0

    do i = 1, size(op_point_specs)
      op_point_specs(i)%opt_type = OPT_TYPES_ALL(i)
      op_point_specs(i)%spec_cl = .true.
      op_point_specs(i)%value = cl_value
      op_point_specs(i)%re%number = 200000d0
      op_point_specs(i)%re%type = 1
      op_point_specs(i)%ma%number = 0d0
      op_point_specs(i)%ma%type = 1
      op_point_specs(i)%ncrit = -1d0
      op_point_specs(i)%target_value = -1d0
      op_point_specs(i)%flap_angle = 0d0
      cl_value = cl_value + 0.1d0
    end do

    do i = 1, size(op_point_specs)
      call print_op_point_spec (op_point_specs(i), with_header=(i == 1), indent=5)
    end do

  end subroutine ensure_op_point_specs



  subroutine ensure_op_point_results ()

    type(airfoil_type)                  :: foil
    type(xfoil_options_type)            :: xfoil_options
    integer                             :: i

    if (allocated(op_point_results)) return

    call ensure_op_point_specs()

    call print_action ("Create op point results ...")

    foil = create_bezier_JX_GS3_100 (161)

    xfoil_options%show_details         = .true.

    call xfoil_cleanup()
    call xfoil_init()

    call run_op_points_no_flap (foil, xfoil_options, op_point_specs, op_point_results)
    
    do i = 1, size(op_point_results)
      call print_op_point_result (op_point_results(i), with_header=(i == 1), indent=5)
    end do
    call xfoil_stats_print(5)
    call xfoil_cleanup()

  end subroutine ensure_op_point_results


  subroutine set_target_value_from_results ()

    ! set test target values in op_point_specs from the actual results in op_point_results

    type(op_point_spec_type) :: op_spec
    type(op_point_result_type) :: op
    integer :: i

    call ensure_op_point_results()

    do i = 1, size(op_point_specs)
      op_spec = op_point_specs(i)
      op = op_point_results(i)

      if (is_target(op_spec)) then 
        select case (op_spec%opt_type)
          case (OPT_TARGET_CL) 
            op_spec%target_value = op%cl * 1.1d0
          case (OPT_TARGET_CD) 
            op_spec%target_value = op%cd * 0.9d0
          case (OPT_TARGET_CM) 
            op_spec%target_value = 0.01d0
          case (OPT_TARGET_GLIDE) 
            op_spec%target_value = (op%cl / op%cd) * 1.1d0
          case default 
            call assert(.false., "Unexpected opt type in set_target_value_from_results")
        end select
      end if
      op_point_specs(i) = op_spec
    end do

  end subroutine set_target_value_from_results


  subroutine test_op_point_specs()

    logical                  :: is_ok
    character(:), allocatable :: opt_name
    integer                  :: i, n_not_ok

    call test_header("Op point basics")

    call ensure_op_point_specs()

    n_not_ok = 0

    do i = 1, size(op_point_specs)
      opt_name = trim(opt_type_name(op_point_specs(i)%opt_type))

      is_ok = opt_type_enum(opt_name) == op_point_specs(i)%opt_type
      is_ok = is_ok .and. (opt_type_strength(op_point_specs(i)%opt_type) > 0d0)

      if (.not. is_ok) then
        n_not_ok = n_not_ok + 1
        call assert(.false., "op_point invariants "//opt_name)
      end if
    end do

    call asserti (n_not_ok, 0, "Create op point specs")

  end subroutine test_op_point_specs


  subroutine test_op_point_results()

    integer :: i, n_bad_drag, n_converged

    call test_header("Op point xfoil run")

    call ensure_op_point_results()

    call asserti(size(op_point_results), size(op_point_specs), "Create op point results")
    call assertf(op_point_results(1)%alpha, -0.999d0, "Alpha at CL=0.0", 3)
    call assertf(op_point_results(3)%cd,     0.00860d0, "CD at CL=0.2", 5)
    call assertf(op_point_results(5)%xtrt,   0.8402d0,  "Top transition at CL=0.4", 4)
    call assertf(op_point_results(9)%cm,    -0.0261d0,  "CM at CL=0.8", 4)

    n_converged = 0
    n_bad_drag = 0
    do i = 1, size(op_point_results)
      if (op_point_results(i)%converged) then
        n_converged = n_converged + 1
        if (op_point_results(i)%cd <= 0d0) n_bad_drag = n_bad_drag + 1
      end if
    end do

    call asserti(n_bad_drag, 0, "Positive drag at converged op points")
    call asserti(n_converged, size(op_point_results), "Converged op point results")

  end subroutine test_op_point_results


  subroutine test_op_point_objective()

    integer :: i, n_invalid
    type(op_point_eval_type)   :: op_eval
    type(op_point_spec_type)   :: tmp_spec
    type(op_point_result_type) :: tmp_result

    call test_header("Op point objective and improvement")
    call print_action ("Initial calculate seed ref values and objective ...")

    call ensure_op_point_results()

    call set_target_value_from_results          ! we need target values for seed_ref

    n_invalid = 0
    do i = 1, size(op_point_results)
      call init_op_point_seed_ref (op_point_specs(i), op_point_results(i))
      call print_op_point_objective (op_point_specs(i), op_point_results(i), with_header=(i == 1), indent=5)
      if (.not. op_point_specs(i)%seed%valid) n_invalid = n_invalid + 1
    end do

    print *
    call print_action ("Results changed by factor ...")

    op_point_results%cd = op_point_results%cd * 0.95d0
    op_point_results%cl = op_point_results%cl * 0.95d0
    op_point_results%cm = op_point_results%cm * 0.5d0
    do i = 1, size(op_point_results)
      call print_op_point_objective (op_point_specs(i), op_point_results(i), with_header=(i == 1), indent=5)
    end do

    tmp_result%converged = .true.
    tmp_spec%target_value = 0d0
    tmp_spec%allow_improved_target = .true.
    tmp_spec%seed%valid = .true.
    tmp_spec%seed%value = 0d0
    tmp_spec%seed%miss = 0d0

    tmp_spec%opt_type = OPT_MAX_CL
    tmp_spec%seed%value = 0.1d0
    tmp_result%cl = 0.095d0
    op_eval = op_point_eval(tmp_spec, tmp_result)
    call assertf (op_eval%improvement, -5d0, "Improvement max-lift normal percent", 6)
    call assert (.not. op_eval%improvement_uses_base, "Improvement max-lift normal uses seed")
    call asserti (op_eval%quality, Q_BAD, "Improvement max-lift normal quality")

    tmp_spec%seed%value = 0d0
    tmp_result%cl = 0d0
    call assertf (op_point_objective(tmp_spec, tmp_result), 1d0, "Objective max-lift zero safe", 6)
    op_eval = op_point_eval(tmp_spec, tmp_result)
    call assertf (op_eval%improvement, 0d0, "Improvement max-lift zero safe", 6)
    call assert (op_eval%improvement_uses_base, "Improvement max-lift zero safe uses base")

    tmp_spec%opt_type = OPT_TARGET_CL
    tmp_result%cl = 0d0
    call assertf (op_point_objective(tmp_spec, tmp_result), 1d0, "Objective target-lift zero safe", 6)
    op_eval = op_point_eval(tmp_spec, tmp_result)
    call assertf (op_eval%improvement, 0d0, "Improvement target-lift zero safe", 6)
    call assert (op_eval%improvement_uses_base, "Improvement target-lift zero safe uses base")

    tmp_spec%opt_type = OPT_TARGET_CM
    tmp_spec%seed%value = 0d0
    tmp_spec%seed%miss = 0d0
    tmp_result%cm = 0d0
    call assertf (op_point_objective(tmp_spec, tmp_result), 1d0, "Objective target-moment zero safe", 6)
    op_eval = op_point_eval(tmp_spec, tmp_result)
    call assertf (op_eval%improvement, 0d0, "Improvement target-moment zero safe", 6)
    call assert (op_eval%improvement_uses_base, "Improvement target-moment zero safe uses base")

    tmp_spec%opt_type = OPT_TARGET_CM
    tmp_spec%allow_improved_target = .true.
    tmp_spec%seed%value = -0.05d0
    tmp_spec%seed%miss = 0.05d0
    tmp_result%cm = -0.02d0
    call assertf (op_point_objective(tmp_spec, tmp_result), 0.625d0, "Objective target-moment higher-better", 6)
    op_eval = op_point_eval(tmp_spec, tmp_result)
    call assertf (op_eval%improvement, 60d0, "Improvement target-moment higher-better", 6)
    call assert (.not. op_eval%improvement_uses_base, "Improvement target-moment higher-better uses seed")

    tmp_spec%target_value = 0.01d0
    tmp_spec%seed%value = -0.0261d0
    tmp_spec%seed%miss = 0.0361d0
    tmp_result%cm = -0.01305d0
    op_eval = op_point_eval(tmp_spec, tmp_result)
    call assertf (op_eval%improvement, 36.1495844875346d0, "Improvement target-moment uses seed miss", 6)
    call assert (.not. op_eval%improvement_uses_base, "Improvement target-moment seed miss uses seed")

    tmp_spec%allow_improved_target = .false.
    tmp_spec%target_value = 0d0
    tmp_spec%seed%value = -0.05d0
    tmp_spec%seed%miss = 0.05d0
    tmp_result%cm = 0.02d0
    call assertf (op_point_objective(tmp_spec, tmp_result), &
          0.625d0, "Objective target-moment symmetric overshoot", 6)
        op_eval = op_point_eval(tmp_spec, tmp_result)
        call assertf (op_eval%improvement, 60d0, "Improvement target-moment symmetric overshoot", 6)
        call assert (.not. op_eval%improvement_uses_base, "Improvement target-moment symmetric overshoot uses seed")

    tmp_spec%opt_type = OPT_TARGET_CD
    tmp_spec%target_value = 0d0
    tmp_spec%seed%value = 0.05d0
    tmp_spec%seed%miss = 0.05d0
    tmp_result%cd = -0.02d0
    call assertf (op_point_objective(tmp_spec, tmp_result), &
      0.4d0 ** opt_type_strength(OPT_TARGET_CD), "Objective target-drag symmetric overshoot", 6)
    op_eval = op_point_eval(tmp_spec, tmp_result)
    call assertf (op_eval%improvement, 60d0, "Improvement target-drag symmetric overshoot", 6)
    call assert (.not. op_eval%improvement_uses_base, "Improvement target-drag symmetric overshoot uses seed")

    tmp_spec%opt_type = OPT_TARGET_GLIDE
    tmp_spec%target_value = 0.1d0
    tmp_spec%seed%value = 0.05d0
    tmp_spec%seed%miss = 0.05d0
    tmp_result%cl = 0.18d0
    tmp_result%cd = 1d0
    call assertf (op_point_objective(tmp_spec, tmp_result), &
      2.5d0 ** opt_type_strength(OPT_TARGET_GLIDE), "Objective target-glide symmetric overshoot", 6)
    op_eval = op_point_eval(tmp_spec, tmp_result)
    call assertf (op_eval%improvement, -60d0, "Improvement target-glide symmetric overshoot", 6)
    call assert (.not. op_eval%improvement_uses_base, "Improvement target-glide symmetric overshoot uses seed")

    tmp_spec%allow_improved_target = .true.
    tmp_spec%target_value = 75d0
    tmp_spec%seed%value = 60d0
    tmp_spec%seed%miss = 15d0
    tmp_result%cl = 79d0
    tmp_result%cd = 1d0
    op_eval = op_point_eval(tmp_spec, tmp_result)
    call assertf (op_eval%target_deviation, 0d0, "Eval target-glide improved target deviation clamps", 6)
    call assertf (op_eval%target_deviation_abs, 0d0, "Eval target-glide improved target deviation abs clamps", 6)
    call assert (op_eval%target_reached, "Eval target-glide improved target reached")
    call asserti (op_eval%quality, Q_GOOD, "Eval target-glide improved target quality")

    tmp_spec%opt_type = OPT_TARGET_GLIDE
    tmp_spec%allow_improved_target = .true.
    tmp_spec%target_value = 70d0
    tmp_spec%seed%value = 75d0
    tmp_spec%seed%miss = -5d0
    tmp_result%cl = 70d0
    tmp_result%cd = 1d0
    call assertf (op_point_objective(tmp_spec, tmp_result), 1d0, &
      "Objective target-glide hit clamps to one", 6)

    tmp_spec%opt_type = OPT_TARGET_CD
    tmp_spec%allow_improved_target = .true.
    tmp_spec%target_value = 0.010d0
    tmp_spec%seed%value = 0.008d0
    tmp_spec%seed%miss = -0.002d0
    tmp_result%cd = 0.010d0
    call assertf (op_point_objective(tmp_spec, tmp_result), 1d0, &
      "Objective target-drag hit clamps to one", 6)

    tmp_spec%target_value = 0.010d0
    tmp_spec%seed%value = 0.012d0
    tmp_spec%seed%miss = 0.002d0
    tmp_result%cd = 0.0095d0
    call assertf (op_point_objective(tmp_spec, tmp_result), 0.833333333333333d0, &
      "Objective target-drag hit freezes at threshold", 6)
    tmp_result%cd = 0.0085d0
    call assertf (op_point_objective(tmp_spec, tmp_result), 0.833333333333333d0, &
      "Objective target-drag better-than-hit stays constant", 6)

    tmp_spec%opt_type = OPT_TARGET_GLIDE
    tmp_spec%allow_improved_target = .true.
    tmp_spec%target_value = 70d0
    tmp_spec%seed%value = 60d0
    tmp_spec%seed%miss = 10d0
    tmp_result%cl = 75d0
    tmp_result%cd = 1d0
    call assertf (op_point_objective(tmp_spec, tmp_result), 0.857142857142857d0, &
      "Objective target-glide hit freezes at threshold", 6)
    tmp_result%cl = 80d0
    call assertf (op_point_objective(tmp_spec, tmp_result), 0.857142857142857d0, &
      "Objective target-glide better-than-hit stays constant", 6)

    tmp_spec%opt_type = OPT_MAX_CL
    tmp_spec%weighting = 0.5d0
    tmp_spec%seed%value = 0.1d0
    tmp_spec%seed%miss = 0d0
    tmp_result%cl = 0.095d0
    op_eval = op_point_eval(tmp_spec, tmp_result)
    call assertf (op_eval%objective, ((1d0 + 0.1d0) / (1d0 + 0.095d0)) * 0.5d0, &
      "Eval max-lift objective contribution", 6)
    call assertf (op_eval%improvement, -5d0, "Eval max-lift improvement percent", 6)
    call assertf (op_eval%improvement_abs, -0.005d0, "Eval max-lift improvement abs", 6)
    call assert (.not. op_eval%improvement_uses_base, "Eval max-lift improvement uses seed")
    call assertf (op_eval%target_deviation, 0d0, "Eval max-lift target deviation", 6)
    call assert (.not. op_eval%target_reached, "Eval max-lift target reached false")

    tmp_spec%opt_type = OPT_TARGET_CD
    tmp_spec%weighting = 2d0
    tmp_spec%target_value = 0.1d0
    tmp_spec%allow_improved_target = .true.
    tmp_spec%seed%value = 0.2d0
    tmp_spec%seed%miss = 0.1d0
    tmp_result%cd = 0.12d0
    op_eval = op_point_eval(tmp_spec, tmp_result)
    call assertf (op_eval%objective, (0.12d0 / 0.2d0) ** opt_type_strength(OPT_TARGET_CD) * 2d0, &
      "Eval target-drag objective contribution", 6)
    call assertf (op_eval%improvement, 40d0, "Eval target-drag improvement percent", 6)
    call assertf (op_eval%improvement_abs, 0.08d0, "Eval target-drag improvement abs", 6)
    call assert (.not. op_eval%improvement_uses_base, "Eval target-drag improvement uses seed")
    call assertf (op_eval%target_deviation, 20d0, "Eval target-drag target deviation", 6)
    call assertf (op_eval%target_deviation_abs, 0.02d0, "Eval target-drag target deviation abs", 6)
    call assert (.not. op_eval%target_reached, "Eval target-drag target reached false")
    call asserti (op_eval%quality, Q_PROBLEM, "Eval target-drag quality")


  end subroutine test_op_point_objective

end module test_op_point