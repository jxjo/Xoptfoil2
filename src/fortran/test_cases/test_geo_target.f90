! MIT License
! Copyright (c) 2026 Jochen Guenzel

module test_geo_target

  use geo_target
  use test_util

  implicit none

contains

  subroutine test_geo_target_all ()
    call test_geo_target_objective()
  end subroutine



  subroutine test_geo_target_objective()

    type(geo_target_type)      :: geo_spec
    type(geo_result_type)      :: geo_result
    type(geo_target_eval_type) :: geo_eval

    call test_header("Geo target objective and evaluation")

    geo_spec%type = GEO_TARGET_THICKNESS
    geo_spec%target_value = 0.12d0
    geo_spec%weighting = 0.5d0
    geo_result%maxt = 0.10d0

    call init_geo_target_seed_ref(geo_spec, geo_result)

    call assertf(geo_target_objective(geo_result, geo_spec), 0.5d0, &
      "Geo objective thickness seed weighted", 6)

    geo_eval = geo_target_eval(geo_spec, geo_result)
    call assertf(geo_eval%objective, 0.5d0, &
      "Geo eval thickness seed objective contribution", 6)

    geo_result%maxt = 0.11d0

    call assertf(geo_target_objective(geo_result, geo_spec), 0.451612903225806d0, &
      "Geo objective thickness weighted away from seed", 6)

    geo_eval = geo_target_eval(geo_spec, geo_result)
    call assertf(geo_eval%objective, 0.451612903225806d0, &
      "Geo eval thickness objective contribution", 6)
    call assertf(geo_eval%target_deviation_abs, -0.01d0, &
      "Geo eval thickness target gap sign", 6)
    call assertf(geo_eval%target_deviation, 8.33333333333333d0, &
      "Geo eval thickness target deviation", 6)

    geo_spec%type = GEO_TARGET_MATCH_FOIL
    geo_spec%target_value = 0d0
    geo_spec%weighting = 2d0
    geo_result%match_top_deviation = 0.03d0
    geo_result%match_bot_deviation = 0.02d0

    call init_geo_target_seed_ref(geo_spec, geo_result)

    call assertf(geo_target_objective(geo_result, geo_spec), 2d0, &
      "Geo objective match-foil seed weighted", 6)

    geo_eval = geo_target_eval(geo_spec, geo_result)
    call assertf(geo_eval%objective, 2d0, &
      "Geo eval match-foil seed objective contribution", 6)

    geo_result%match_top_deviation = 0.015d0
    geo_result%match_bot_deviation = 0.01d0

    call assertf(geo_target_objective(geo_result, geo_spec), 1d0, &
      "Geo objective match-foil weighted away from seed", 6)

    geo_eval = geo_target_eval(geo_spec, geo_result)
    call assertf(geo_eval%objective, 1d0, &
      "Geo eval match-foil objective contribution", 6)

  end subroutine test_geo_target_objective

end module test_geo_target