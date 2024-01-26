! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2022-2024 Jochen Guenzel

module optimization_driver

! Entry for optimization - starts either PSO, Generic ... 

  use os_util
  use particle_swarm,     only : pso_options_type
  use genetic_algorithm,  only : ga_options_type
  use simplex_search,     only : simplex_options_type


  implicit none
  private

  integer, parameter, public  :: PSO      = 1            ! optimization search types 
  integer, parameter, public  :: GENETIC  = 2
  integer, parameter, public  :: SIMPLEX  = 3

  public :: optimize 
  public :: optimize_spec_type
  public :: write_final_design

  ! Main specification of optimization / options 

  type optimize_spec_type   
    integer                       :: type                 ! type of optimization  
    type(pso_options_type)        :: pso_options
    type(ga_options_type)         :: ga_options
    type(simplex_options_type)    :: sx_options
  end type optimize_spec_type



  ! --------- private --------------------------------------------------------

  contains


  subroutine optimize(optimize_options, optdesign, f0_ref, fmin, steps, fevals)

    !----------------------------------------------------------------------------
    !! Controler for optimization process - calls either PSO or Genetic 
    !----------------------------------------------------------------------------

    ! use commons,             only : nflap_optimize,              &
    !                                min_flap_degrees,            &
    !                                max_flap_degrees, flap_degrees,               &
    !                                flap_optimize_points,          &
    !                                seed_foil 
    use particle_swarm,     only : particleswarm
    use genetic_algorithm,  only : geneticalgorithm
    use simplex_search,     only : simplexsearch
    use eval,               only : objective_function,                           &
                                   objective_function_nopenalty, write_progress
    
    use shape_airfoil,      only : shaping, BEZIER, HICKS_HENNE, CAMB_THICK 
    use shape_airfoil,      only : designvars_0

    type(optimize_spec_type), intent(in) :: optimize_options
    double precision, allocatable, intent(out) :: optdesign (:) 
    double precision, intent(out) :: f0_ref, fmin
    integer, intent(out) :: steps, fevals

    double precision, allocatable  :: x0 (:) 
    logical :: initial_x0_based
    integer :: ndv_shape, ndv
    integer :: stepsg, fevalsg, stepsl, fevalsl, designcounter

    stepsg = 0
    fevalsg = 0
    stepsl = 0
    fevalsl = 0
    designcounter = 0

    if (shaping%type == BEZIER) then 
      ndv = shaping%bezier%ndv
    elseif (shaping%type == HICKS_HENNE) then 
      ndv = shaping%hh%ndv
    else 
      ndv = shaping%camb_thick%ndv
    end if 

    allocate (x0(ndv))
    if (allocated(optdesign)) then 
      deallocate (optdesign)
    end if 
    allocate (optdesign(ndv))

    ! Set initial design = seed airfoil 

    ndv_shape = ndv  ! - nflap_optimize
    x0 (1:ndv_shape) = designvars_0 ()

    ! Seed flap deflection as specified in input file
      ! do i = ndv_shape + 1, ndv
      !   oppoint = flap_optimize_points(i-ndv_shape)
      !   x0(i) = flap_degrees(oppoint)*ffact
      ! end do

      ! do i = 3*ndv_shape+1, ndv                     ! eg flap 1° - 10° 
      !   xmin(i) = min_flap_degrees*ffact            !            0.0001
      !   xmax(i) = max_flap_degrees*ffact            !            0.001 
      ! end do

    ! Compute f0_ref, ignoring penalties for violated constraints

    f0_ref = objective_function_nopenalty(x0) 

    ! Write seed airfoil coordinates and polars to file

    call write_progress (x0, 0) 

    ! Set up mins and maxes

    ! #todo - remove 
    
    if (shaping%type == CAMB_THICK) then

      initial_x0_based = .false.                     ! inital designs will between 0 and 1

    elseif (shaping%type == BEZIER) then

      initial_x0_based = .true.                     ! inital designs will be close to x0

    else      ! Hicks-Henne 

      initial_x0_based = .false.                     ! inital designs will between 0 and 1

    end if

    ! Finally - do optimization -----------

    if (optimize_options%type == PSO) then

      call particleswarm(optdesign, fmin, stepsg, fevalsg, objective_function, &
                          x0, initial_x0_based, &
                          f0_ref,  &
                          optimize_options%pso_options, designcounter)

    else if (optimize_options%type == GENETIC) then

      ! #todo remove xmin, xmax in genetic
      ! call geneticalgorithm(optdesign, fmin, stepsg, fevalsg, objective_function, &
      !                       x0, initial_x0_based, &
      !                       .true., f0_ref, constrained_dvs, ga_options,               &
      !                       designcounter)

    end if

    ! Total number of steps and function evaluations

    steps = stepsg + stepsl
    fevals = fevalsg + fevalsl

  end subroutine optimize




  subroutine write_final_design(optdesign, f0, fmin, final_airfoil)

    !-----------------------------------------------------------------------------
    !! Writes final airfoil design to a file
    !!    Returns final airfoil 
    !-----------------------------------------------------------------------------

    use commons
    use airfoil_operations,     only : airfoil_write
    use xfoil_driver,           only : run_op_points, op_point_result_type
    use xfoil_driver,           only : op_point_specification_type
    use eval,     only : create_airfoil_from_designvars, get_flap_degrees_from_design
    use eval_commons,     only : xfoil_geom_options, xfoil_options, noppoint
    use eval_commons,     only : op_points_spec, match_foils
    use shape_bezier,           only : write_bezier_file
    use shape_hicks_henne,      only : write_hh_file

    double precision, dimension(:), intent(in) :: optdesign
    double precision, intent(in)               :: f0, fmin
    type(airfoil_type), intent(out)            :: final_airfoil

    type(op_point_specification_type) :: op_spec
    type(op_point_result_type)        :: op
    type(op_point_result_type), dimension(:), allocatable :: op_points_result
    double precision, dimension(noppoint) :: actual_flap_degrees
    integer :: i, iunit
    character(:), allocatable :: output_file, aero_file
    character(20) :: flapnote
    double precision :: ncrit

    
    ! Rebuild foil out final design and seed airfoil

    call create_airfoil_from_designvars (optdesign, final_airfoil)
    
    final_airfoil%name   = output_prefix

    ! Use Xfoil to analyze final design

    if (.not. match_foils) then

      ! Get actual flap angles based on design variables

      call get_flap_degrees_from_design (optdesign, actual_flap_degrees)

      ! Run xfoil for requested operating points

      call run_op_points (final_airfoil, xfoil_geom_options, xfoil_options,        &
                          flap_spec, actual_flap_degrees,  &
                          op_points_spec, op_points_result)

      ! Write summary to screen and file

      aero_file  = design_subdir//'Performance_Summary.dat'

      iunit = 13
      open(unit=iunit, file=aero_file, status='replace')

      write(*,*)
      write(*    ,'(A)') " Optimal airfoil performance summary"
      write(iunit,'(A)') " Optimal airfoil performance summary"
      write(*    ,'(A)') ""
      write(iunit,'(A)') ""

      ! i    alpha     CL        CD       Cm    Top Xtr Bot Xtr   Re      Mach     ncrit     flap 
      ! --  ------- -------- --------- -------- ------- ------- -------- -------- ------- -----------
      !  7  -1.400   0.0042   0.00513  -0.0285  0.7057  0.2705  6.00E+04   0.000     9.1    5.23 spec
      ! I2    F8.3    F9.4     F10.5     F9.4    F8.4    F8.4     ES9.2     F8.3     F7.1    F6.2  

      write (iunit,'(A)') " i   alpha     CL        CD       Cm    Top Xtr Bot Xtr   Re      Mach    ncrit     flap"
      write (iunit,'(A)') " -- ------- -------- --------- -------- ------- ------- ------- -------- ------- -----------"
      write (*    ,'(A)') " i   alpha     CL        CD       Cm    Top Xtr Bot Xtr   Re      Mach    ncrit     flap"
      write (*    ,'(A)') " -- ------- -------- --------- -------- ------- ------- ------- -------- ------- -----------"

      do i = 1, noppoint

        op_spec  = op_points_spec(i)
        op       = op_points_result(i) 

        if (flap_spec%use_flap) then
          write (flapnote, '(F6.2)') actual_flap_degrees(i)
          if (flap_selection(i) == "specify") then
            flapnote = trim(flapnote) //" spec"
          else
            flapnote = trim(flapnote) //" opt"
          end if 
        else
          flapnote = "   -"
        end if 

        if (op_spec%ncrit == -1d0) then 
          ncrit = xfoil_options%ncrit
        else
          ncrit = op_spec%ncrit
        end if 

        write (iunit,  "(I2,   F8.3,   F9.4,    F10.5, F9.4,   F8.4,   F8.4, ES9.2     F8.3     F7.1, 3X, A)") &
          i, op%alpha, op%cl, op%cd, op%cm, op%xtrt, op%xtrb, &
          op_spec%re%number, op_spec%ma%number, ncrit, trim(flapnote)
        write (*    ,  "(I2,   F8.3,   F9.4,    F10.5, F9.4,   F8.4,   F8.4, ES9.2     F8.3     F7.1, 3X, A)") &
          i, op%alpha, op%cl, op%cd, op%cm, op%xtrt, op%xtrb, &
          op_spec%re%number, op_spec%ma%number, ncrit, trim(flapnote)

      end do

      write(*,*)
      write(*,'(A,F8.4,A1)', advance='no') " Objective function improvement over seed: "
      call  print_colored_r (9, '(F8.4,"%")', Q_GOOD, ((f0 - fmin)/f0*100.d0))

      write(iunit,*)
      write(iunit,'(A43F8.4A1)') " Objective function improvement over seed: ",  &
                            (f0 - fmin)/f0*100.d0, "%" 

      close(iunit)

      write(*,*)
      call print_text ("- Writing summary to "//trim(aero_file))


    else
      call write_matchfoil_summary (final_airfoil)
    end if

    ! Write airfoil to file

    output_file = output_prefix//'.dat'
    call airfoil_write (output_file, output_prefix, final_airfoil)

    if (final_airfoil%is_bezier_based) then
      output_file = output_prefix//'.bez'
      call print_colored (COLOR_NOTE, "   Writing bezier  to ")
      call print_colored (COLOR_HIGH, output_file)
      write (*,*)
      call write_bezier_file (output_file, output_prefix, final_airfoil%top_bezier, final_airfoil%bot_bezier)

    else if (final_airfoil%is_hh_based) then
      output_file = output_prefix//'.hicks'
      call print_colored (COLOR_NOTE, "   Writing hicks   to ")
      call print_colored (COLOR_HIGH, output_file)
      write (*,*)
      call write_hh_file (output_file, output_prefix, final_airfoil%top_hh, final_airfoil%bot_hh)
    end if 

  end subroutine write_final_design



  subroutine write_matchfoil_summary (final_foil)

    !-----------------------------------------------------------------------------
    !! Write some data of the final match foil 
    !-----------------------------------------------------------------------------

    use commons,            only : airfoil_type
    use eval_commons,       only : foil_to_match
    use shape_airfoil,      only : get_seed_foil
    use math_deps,          only : median
    use xfoil_driver,       only : xfoil_set_airfoil, xfoil_get_geometry_info

    type (airfoil_type), intent(in)  :: final_foil


    type (airfoil_type)   :: seed_foil
    double precision :: max_dzt, max_dzb, avg_dzt, avg_dzb, max_dzt_rel, max_dzb_rel
    double precision :: xmax_dzt, xmax_dzb, median_dzt, median_dzb
    double precision :: maxt, xmaxt, maxc, xmaxc
    integer          :: imax_dzt, imax_dzb

    ! Max Delta and position on top and bot 

    max_dzt  = maxval(abs (final_foil%top%y - foil_to_match%top%y))
    max_dzb  = maxval(abs (final_foil%bot%y - foil_to_match%bot%y))
    imax_dzt = maxloc(abs (final_foil%top%y - foil_to_match%top%y),1)
    imax_dzb = maxloc(abs (final_foil%bot%y - foil_to_match%bot%y),1)


    call get_seed_foil (seed_foil) 

    xmax_dzt = seed_foil%top%x(imax_dzt)
    xmax_dzb = seed_foil%bot%x(imax_dzb)

    ! rel. deviation  

    call xfoil_set_airfoil (foil_to_match)        
    call xfoil_get_geometry_info (maxt, xmaxt, maxc, xmaxc)

    max_dzt_rel = (max_dzt / maxt) * 100.d0
    max_dzb_rel = (max_dzb / maxt) * 100.d0
  
    ! absolute average and median of deltas  

    avg_dzt  = sum (abs(final_foil%top%y - foil_to_match%top%y)) / size(seed_foil%top%x,1)
    avg_dzb  = sum (abs(final_foil%bot%y - foil_to_match%bot%y)) / size(seed_foil%bot%x,1)

    median_dzt  = median (final_foil%top%y - foil_to_match%top%y)
    median_dzb  = median (final_foil%bot%y - foil_to_match%bot%y)

    write(*,*)
    write(*,'(A)') " Match airfoil deviation summary"
    write(*,*)
    write(*,'(A)') "      Delta of y-coordinate between best design and match airfoil surface"
    write(*,*)
    write(*,'(A)') "               average      median   max delta      at  of thickness"

    write (*,'(A10)', advance = 'no') "   top:"
    write (*,'(F12.7,F12.7,F12.7,F8.4,F10.3,A1)') avg_dzt, median_dzt, max_dzt, xmax_dzt, max_dzt_rel,'%'
    write (*,'(A10)', advance = 'no') "   bot:"
    write (*,'(F12.7,F12.7,F12.7,F8.4,F10.3,A1)') avg_dzb, median_dzb, max_dzb, xmax_dzb, max_dzb_rel,'%'
    write(*,*)

  end subroutine write_matchfoil_summary

end module optimization_driver
