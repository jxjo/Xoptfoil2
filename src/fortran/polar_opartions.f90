!  This file is part of XOPTFOIL-JX.

!  XOPTFOIL-JX is a modified version of ...
!  XOPTFOIL is free software: you can redistribute it and/or modify
!  it under the terms of the GNU General Public License as published by
!  the Free Software Foundation, either version 3 of the License, or
!  (at your option) any later version.

!  Copyright (C) XOPTFOIL 2017-2019 Daniel Prosser
!  Copyright (C) XOPTFOIL-JX 2019-2023 Jochen Guenzel

module polar_operations

  ! create and write xfoil based polars

  use os_util
  use print_util
  
  use xfoil_driver,       only : re_type, op_point_spec_type
  use xfoil_driver,       only : op_point_result_type


  implicit none

  type polar_type
    character(250)   :: airfoil_name    ! Name of airfoil
    character(250)   :: file_name       ! Name of polar file name 
    character(250)   :: add_info        ! additional info string in polar file 
    type(re_type)    :: re              ! Re number of this polar (re*sqrt(cl) if Type2)
    type(re_type)    :: ma              ! Ma number of this polar (mach*sqrt(cl) if Type2)
    double precision :: ncrit           ! ncrit of polar
    logical          :: spec_cl         ! base value of polar either cl or alpha
    double precision :: start_value     ! polar starting from ...
    double precision :: end_value       ! ... to end value ...
    double precision :: increment       ! ... incremented by 
    integer :: n_op_points              ! number of all op_poins of this polar
    type(op_point_spec_type), dimension (:), allocatable :: &
                        op_points_spec  !array with specified op_points
    type(op_point_result_type), dimension (:), allocatable :: & 
                        op_points       !array with all calculated op_points
  end type polar_type

  integer, parameter :: MAXPOLARS = 30            ! max number of polars
  integer, parameter :: MAXOPS    = 100           ! max number of operating points of a polar 

  character(1), parameter    :: DELIMITER = ','


   ! Parms for operating point specification
  integer, private :: npolars
  type (polar_type), dimension (MAXPOLARS), private :: polars


contains

 
!=============================================================================
! Generate and write to file all 'npolars' 'polars' for an airfoil
!
! Each polar will be written in a single file in xfoil text format
! in the subdirectory 'foilname_polars'.
!
! The name of the file is aligned to xflr5 polar file naming
!=============================================================================

subroutine generate_polar_files (show_details, subdirectory, foil, xfoil_options)

  use airfoil_base,       only : airfoil_type, panel_options_type
  use os_util,            only : make_directory
  use xfoil_driver,       only : xfoil_options_type
  use xfoil_driver,       only : op_point_result_type, run_op_points 
  use xfoil_driver,       only : flap_spec_type

  type (airfoil_type), intent (in)  :: foil
  logical, intent(in)               :: show_details
  character (*), intent(in)         :: subdirectory
  type (xfoil_options_type), intent(in) :: xfoil_options

  type (xfoil_options_type)         :: local_xfoil_options
  double precision, allocatable     :: flap_degrees (:)
  type(flap_spec_type)              :: flap_spec               ! dummy - no flaps used
  type(op_point_result_type), allocatable :: op_points_result (:)
  integer :: i
  character (255) :: polars_subdirectory, polar_label

  flap_spec%use_flap  = .false. 
  local_xfoil_options = xfoil_options
  local_xfoil_options%exit_if_unconverged = .false.  ! we need all op points
  local_xfoil_options%detect_outlier      = .false.  ! makes no sense for polar calcualtion being excuted only once
  local_xfoil_options%show_details = show_details

  if (trim(subdirectory) == '' ) then 
    polars_subdirectory = ''
  else
    polars_subdirectory = trim(subdirectory) // '\'
  end if

  ! flaps degrees will be 0

  allocate (flap_degrees(polars(i)%n_op_points))
  flap_degrees (:)    = 0.d0 

  ! calc and write all polars

  do i = 1, npolars

    if (allocated(op_points_result))  deallocate (op_points_result)
   
    if (show_details) then 
      write (polar_label,'(A, I7)') 'Re=',  int(polars(i)%re%number)
      call print_text ('- Generating polar ' // trim(polar_label) // '  ')
    end if 

    call run_op_points (foil, local_xfoil_options,        &
                        flap_spec, flap_degrees, &
                        polars(i)%op_points_spec, op_points_result)
  

    if (show_details) then 
      call print_colored (COLOR_NOTE,' - Writing polar ' // trim(polar_label) &
                                    //' to '//trim(polars_subdirectory) //'   ')
    end if 

    open(unit=13, file= trim(polars_subdirectory)//trim(polars(i)%file_name), status='replace')
    call write_polar_header (13, polars(i))
    call write_polar_data   (show_details, 13, polars(i), op_points_result)
    close (13)

  end do 

end subroutine generate_polar_files


 
!=============================================================================
!
! * multi threaded version of generate_polar_files * 
! * only for 'worker' * 
!
! Generate and write to file all 'npolars' 'polars' for an airfoil
!
! Each polar will be written in a single file in xfoil text format
! in the subdirectory 'foilname_polars'.
!
! The name of the file is aligned to xflr5 polar file naming
!=============================================================================

subroutine generate_polar_set (show_details, csv_format, subdirectory, foil, &
                               flap_spec, degrees, xfoil_options)

  use airfoil_base,       only : airfoil_type
  use os_util,            only : make_directory
  use xfoil_driver,       only : xfoil_options_type
  use xfoil_driver,       only : op_point_result_type, run_op_points 
  use xfoil_driver,       only : xfoil_init, xfoil_cleanup
  use xfoil_driver,       only : flap_spec_type

  type (airfoil_type), intent (in)  :: foil
  logical, intent(in)               :: show_details
  logical, intent(in)               :: csv_format
  character (*), intent(in)         :: subdirectory
  type(flap_spec_type), intent(in)           :: flap_spec  
  double precision, intent(in)               :: degrees (:) 
  type (xfoil_options_type), intent(in)      :: xfoil_options

  type(xfoil_options_type)          :: local_xfoil_options
  double precision, allocatable     :: flap_degrees (:)
  type(op_point_result_type), allocatable :: op_points_result (:)
  integer :: i, j, nflap_degress
  character (255) :: polars_subdirectory,  polar_label, polar_action, polar_path
  logical :: exist

! Init flap handling - if no flap set dummy
  if (flap_spec%use_flap) then
    nflap_degress = size(degrees)
  else
    nflap_degress = 1
  end if 

  local_xfoil_options = xfoil_options
  local_xfoil_options%exit_if_unconverged = .false.  ! we need all op points
  local_xfoil_options%detect_outlier      = .false.  ! makes no sense for polar calcualtion being excuted only once
  local_xfoil_options%maxit               = 100       ! increase default value of xfoil iterations

  if (npolars == 1) then
    local_xfoil_options%show_details = show_details
  else
    local_xfoil_options%show_details = .false.        ! multi thread would mix up output
  end if 

  if (trim(subdirectory) == '' ) then 
    polars_subdirectory = ''
  else
    polars_subdirectory = trim(subdirectory) // '\'
  end if

  call xfoil_cleanup()                                ! if xfoil_init was done already


! Multi threaded polars   
!$omp parallel do schedule(static) collapse(2) private(i, j, flap_degrees,op_points_result, polar_label) 

  do j = 1, nflap_degress
    do i = 1, npolars

      if (allocated(flap_degrees)) deallocate (flap_degrees)
      allocate (flap_degrees(polars(i)%n_op_points))
      flap_degrees = degrees(j)

      if (flap_spec%use_flap) then 
        write (polar_label,'(A, F4.1, A, I7)') 'for Flap', degrees(j), &
                           '  Re=',  int(polars(i)%re%number)
      else
        write (polar_label,'(A,I1,A, I7)') 'Type ',polars(i)%re%type,', Re=',  int(polars(i)%re%number)
      endif 

      call print_text ('- Generating polar ' // trim(polar_label) // '  ')


      call xfoil_init()
      call run_op_points (foil, local_xfoil_options, flap_spec, flap_degrees, &
                          polars(i)%op_points_spec, op_points_result) 
      call xfoil_cleanup()
  
!$omp critical (file_write)

      polar_path   = trim(polars_subdirectory) // trim(polars(i)%file_name)
      polar_action = 'Writing polar'

      if (.not. csv_format) then 
        call print_colored (COLOR_NORMAL,' - '// trim(polar_action)//' ' // trim(polar_label) // &
                                        ' to '//trim(polar_path) // ' ')
        open(unit=13, file= trim(polar_path), status='replace')
        call write_polar_header (13, polars(i))
        call write_polar_data   (show_details, 13, polars(i), op_points_result)

      else 

        inquire(file=trim(polar_path), exist=exist)
        if (exist) then       ! append other polars to the file 
          open(unit=13, file= trim(polar_path), status='old', position='append')
          polar_action = 'Appending polar'
        else                  ! csv Header only for new file at the beginning
          open(unit=13, file= trim(polar_path), status='new')
          call write_polar_header_csv (13)
        end if
        call print_colored (COLOR_NORMAL,' - '// trim(polar_action)//' ' // trim(polar_label) // &
                                        ' to '//trim(polar_path) // ' ')
        call write_polar_data_csv (show_details, 13, degrees(j), polars(i), op_points_result)
      end if 
      close (13)
!$omp end critical (file_write)

    end do 
  end do 
!$omp end parallel do

end subroutine generate_polar_set

!=============================================================================
! Read xoptfoil input file to get polars (definition)
!   (separated from read_inputs to be more modular)
! Init polar data structure in this odule
!
!   - re_default for polar definitions with no Reynolds
!   - nrit for the polar xfoil calculation
!   - name of foil
! Returns:  - Polar has to be generated 
!=============================================================================

subroutine read_init_polar_inputs  (iunit, re_default, ncrit, foil_name, &
                                    csv_format, generate_polar) 

  use xfoil_driver,       only : xfoil_options_type
  use input_read,       only : namelist_check

  integer, intent(in)           :: iunit
  type (re_type), intent(in)    :: re_default
  double precision, intent(in)  :: ncrit 
  character(*), intent(in)      :: foil_name
  logical, intent(in)           :: csv_format         
  logical, intent(out)          :: generate_polar           

  integer         :: type_of_polar                           ! 1 or 2 
  character (7)   :: op_mode                                 ! 'spec-al' 'spec_cl'
  double precision, dimension (MAXPOLARS) :: polar_reynolds  ! 40000, 70000, 100000
  double precision, dimension (3)  :: op_point_range         ! -1.0, 10.0, 0.5

  integer :: iostat1, i

  namelist /polar_generation/ generate_polar, type_of_polar, polar_reynolds,   &
                              op_mode, op_point_range

! Init default values for polars

  npolars         = 0
  generate_polar  = .true.
  type_of_polar   = -1
  op_mode         = 'spec-al'
  op_point_range  = (/ -2d0, 10d0 , 1.0d0 /)
  polar_reynolds  = 0d0

! Open input file and read namelist from file

  if (iunit > 0) then
    rewind (iunit)
    read (iunit, iostat=iostat1, nml=polar_generation)
    call namelist_check('polar_generation', iostat1, 'warn')
  end if


  if (.not. generate_polar) return 

! if there are no re numbers in input file take default
  if (polar_reynolds(1) == 0d0) then 
    polar_reynolds(1) = re_default%number 
  end if
  if (type_of_polar == -1) then 
    type_of_polar     = re_default%type
  end if 


! Input sanity

  if ((op_mode /= 'spec-al') .and. (op_mode /= 'spec-cl')) then
    call my_stop ("&polar_generation: op_mode must be 'spec-cl' or 'spec-al'")
  end if
  if ((type_of_polar /= 1) .and. (type_of_polar /= 2)) then 
    call my_stop ("&polar_generation: Type of polars must be either '1' or '2'")
  end if 
  if ((op_point_range(2) - op_point_range(1)) <= 0d0 ) then 
    call my_stop ("&polar_generation: End of polar op_point_range must be higher than the start.")
  end if
  if (( op_point_range(1) + op_point_range(3)) >= op_point_range(2) ) then 
    call my_stop ("&polar_generation: Start of polar op_point_range + increment should be end of op_point_range.")
  end if


! Init polar definitions with input 

  npolars = 0
  do i = 1, size(polar_reynolds)
    if (polar_reynolds(i) > 1000d0) then 
      npolars = npolars + 1
      polars(npolars)%airfoil_name    = trim(foil_name)
      polars(npolars)%add_info        = ''
      polars(npolars)%spec_cl         = (op_mode == 'spec-cl')
      polars(npolars)%start_value     = op_point_range (1)
      polars(npolars)%end_value       = op_point_range (2)
      polars(npolars)%increment       = op_point_range (3)
      polars(npolars)%ma%number       = 0.0d0                   ! currently not supported
      polars(npolars)%ma%type         = 1                       ! currently not supported
      polars(npolars)%re%number       = polar_reynolds(i)
      polars(npolars)%re%type         = type_of_polar
      polars(npolars)%ncrit           = ncrit

      if (csv_format) then
        polars(npolars)%file_name = trim(polars(npolars)%airfoil_name)//'.csv'
      else
      ! build this special xflr5 filename  T1_Re0.400_M0.00_N9.0.txt 
        polars(npolars)%file_name = build_filename (polars(npolars))
      end if

    end if
  end do

  if (npolars > 0) then 
    call init_polars
  else
    call my_stop ("&polar_generation: No Reynolds number found - either in input file nor as command line parameter.")
  end if 

end subroutine read_init_polar_inputs


!=============================================================================
! Initialize the global polar data structure 
!=============================================================================

subroutine init_polars ()

  integer :: ipol, i
  double precision :: cur_value

  do ipol = 1, npolars

  ! calc number of op_points 

    polars(ipol)%n_op_points = get_n_op_points (polars(ipol)) 
    if (polars(ipol)%n_op_points >= 0) then
      if (allocated(polars(ipol)%op_points))      deallocate (polars(ipol)%op_points)
      if (allocated(polars(ipol)%op_points_spec)) deallocate (polars(ipol)%op_points_spec)
      ! only spec - op_points will be allocated in xfoil driver ...
      allocate (polars(ipol)%op_points_spec(polars(ipol)%n_op_points))
    else
      call my_stop ("No valid value boundaries for polar")
    endif

  ! init op data points of polar
  !
  ! if polar is running from eg -5 to +10, split the polar in
  !   0     ... -5
  !   (0+x) ... + 10
  ! to ensure xfoil convergence (starting with a high negative value is critical)

    i = 0 

    if (polars(ipol)%start_value < 0d0) then 

    ! go downward from smallest-1 to start value 

      if (polars(ipol)%end_value > 0d0) then 
        cur_value =  smallest_op_point (polars(ipol)) - polars(ipol)%increment
      else 
        cur_value =  smallest_op_point (polars(ipol)) 
      end if 

      do while (cur_value >= polars(ipol)%start_value)
        i = i + 1
        polars(ipol)%op_points_spec(i)%value    = cur_value
        polars(ipol)%op_points_spec(i)%spec_cl  = polars(ipol)%spec_cl
        polars(ipol)%op_points_spec(i)%re       = polars(ipol)%re
        polars(ipol)%op_points_spec(i)%ma       = polars(ipol)%ma
        polars(ipol)%op_points_spec(i)%ncrit    = polars(ipol)%ncrit
        cur_value = cur_value - polars(ipol)%increment
      end do       
    end if 

    if (polars(ipol)%end_value > 0d0) then 

      ! go upward from smallest to end value 

      cur_value = smallest_op_point (polars(ipol))
      do while (cur_value <= polars(ipol)%end_value + 1.d-6)
        i = i + 1
        polars(ipol)%op_points_spec(i)%value    = cur_value
        polars(ipol)%op_points_spec(i)%spec_cl  = polars(ipol)%spec_cl
        polars(ipol)%op_points_spec(i)%re       = polars(ipol)%re
        polars(ipol)%op_points_spec(i)%ma       = polars(ipol)%ma
        polars(ipol)%op_points_spec(i)%ncrit    = polars(ipol)%ncrit
        cur_value = cur_value + polars(ipol)%increment
      end do       
    end if 

  end do

end subroutine init_polars

!------------------------------------------------------------------------------
! Set info for polar filename and additional info like "Design 123"
!------------------------------------------------------------------------------
subroutine set_polar_info (foil_name, file_name, add_info)

  character (*), intent(in) :: foil_name, file_name, add_info

  integer     :: i

  if (size(polars) > 0) then 
    do i = 1, size(polars)
      if (trim(foil_name) /= '') polars(i)%airfoil_name = trim(foil_name)
      if (trim(file_name) /= '') polars(i)%file_name = trim(file_name)
      if (trim(add_info)  /= '') polars(i)%add_info  = trim(add_info)
    end do 
  else
    call my_stop("No polar initialized for generate_polar.")
  end if 

end subroutine set_polar_info


!------------------------------------------------------------------------------
! Write polar data in xfoil format to out_unit
!------------------------------------------------------------------------------
subroutine write_polar_data (show_details, out_unit, polar, op_points_result)

  use xfoil_driver,       only : op_point_result_type, op_point_spec_type

  logical,              intent(in)  :: show_details
  type (op_point_result_type), dimension (:), intent (in) :: op_points_result
  integer,              intent (in) :: out_unit
  type (polar_type),    intent (in) :: polar

  type (op_point_result_type), dimension (:), allocatable  :: op_points_sorted
  type (op_point_result_type) :: op
  integer              :: i 
  logical              :: has_warned

  has_warned = .false.

! Sort op points again ascending - they were spilt from 0 down and up 

  call sort_op_points (show_details, polar%spec_cl, op_points_result, op_points_sorted, has_warned) 
  if (has_warned) call print_colored (COLOR_NOTE," not converged - skipped")
  write (*,*) 

! xflr5 example
! -
!  alpha     CL        CD       CDp       Cm    Top Xtr Bot Xtr   Cpmin    Chinge    XCp    
! ------- -------- --------- --------- -------- ------- ------- -------- --------- ---------
!  -1.400   0.0042   0.00513   0.00057  -0.0285  0.7057  0.2705  -0.9363   0.0000   7.0438
!   F8.3    F9.4     F10.5     F10.5     F9.4    F8.4    F8.4     F9.4     F9.4     F9.4     

  write (out_unit,'(A)') "  alpha     CL        CD       CDp       Cm    Top Xtr Bot Xtr "
  write (out_unit,'(A)') " ------- -------- --------- --------- -------- ------- ------- "

  do i = 1, size(op_points_sorted)
    op = op_points_sorted(i)
      write (out_unit,  "(   F8.3,   F9.5,    F10.6,    F10.5,    F9.5,   F8.4,   F8.4)") &
                          op%alpha, op%cl, op%cd,    0d0,  op%cm,op%xtrt,op%xtrb
  end do 

end subroutine write_polar_data


!------------------------------------------------------------------------------
! Write polar data of foil in csv format to out_unit
!------------------------------------------------------------------------------
subroutine write_polar_data_csv (show_details, out_unit, flap_degree, polar, op_points_result)

  use xfoil_driver,       only : op_point_result_type

  logical,              intent(in)  :: show_details
  integer,              intent(in)  :: out_unit
  double precision,     intent(in)  :: flap_degree
  type (polar_type),    intent(in)  :: polar
  type (op_point_result_type), dimension (:), intent (in) :: op_points_result

  type (op_point_result_type), dimension (:), allocatable :: op_points_sorted
  type (op_point_result_type)       :: op, op_iminus1
  integer              :: i 
  character (5  )      :: spec
  logical              :: has_warned
  double precision     :: at_cl, cd_invers

  has_warned = .false.

  
! Sort op points again ascending - they were spilt from 0 down and up 
! Remove not converged op oppoints

  call sort_op_points (show_details, polar%spec_cl, op_points_result, op_points_sorted, has_warned) 
  if (has_warned) then 
    call print_colored (COLOR_NOTE," not converged - skipped")
  end if 

! Write to file 

  do i = 2, size(op_points_sorted)

    op = op_points_sorted(i)

    if (polar%spec_cl) then 
      spec = 'cl'
    else 
      spec = 'alpha'
    end if 
    op_iminus1 = op_points_sorted(i-1)
    cd_invers  =  (op%cl - op_iminus1%cl) / ((op%cd + op_iminus1%cd) / 2d0) 
    at_cl          = (op_iminus1%cl + op%cl) / 2d0

    if (op_iminus1%cl > op%cl) exit       ! cl max reached

    write (out_unit,'(A,A, F5.1, A, F7.0,A, F4.1,A, A,A, F7.2,A, F7.3,A, F7.5,A, F6.2,A, F7.4,A, F6.3,A, F6.3,A, F8.5,A, F7.3)') &
                  trim(polar%airfoil_name), DELIMITER, & 
                  flap_degree,              DELIMITER, & 
                  polar%re%number,          DELIMITER, & 
                  polar%ncrit,              DELIMITER, & 
                  trim(spec),               DELIMITER, &      
                  op%alpha,                 DELIMITER, &      
                  op%cl,                    DELIMITER, &      
                  op%cd,                    DELIMITER, &      
                  op%cl / op%cd,            DELIMITER, &      
                  op%cm,                    DELIMITER, &      
                  op%xtrt,                  DELIMITER, &      
                  op%xtrb,                  DELIMITER, & 
                  cd_invers,                DELIMITER, & 
                  at_cl
  end do 

  write (*,*) 
  
end subroutine write_polar_data_csv

!------------------------------------------------------------------------------
! Print the value of failed op during writing output data 
!------------------------------------------------------------------------------
subroutine print_failed_op_value (op, spec_cl, has_warned)

  use xfoil_driver,       only : op_point_result_type

  type (op_point_result_type),intent (in) :: op
  logical,              intent(in)     :: spec_cl
  logical,              intent(inout)  :: has_warned

  character (100)       :: text_out
  character (5  )       :: spec
  double precision      :: val

  if (spec_cl) then 
    spec = "cl"
    val  = op%cl
  else
    spec = "alpha"
    val  = op%alpha
  end if

  if (.not. op%converged) then
    if (.not. has_warned) then 
      write(text_out,'(A)') trim(spec) // "="    // strf('(F5.2)',val)
    else 
      write(text_out,'(A)') "," // strf('(F5.2)',val)
    end if 
    call print_colored (COLOR_NOTE, trim(text_out))
    has_warned = .true. 
  end if
  
end subroutine print_failed_op_value

!------------------------------------------------------------------------------
! Write polar header csv d
!------------------------------------------------------------------------------
subroutine write_polar_header_csv (out_unit)

  integer,           intent (in) :: out_unit

  write (out_unit,'(A)')  "Airfoil"   // DELIMITER //&
                          "Flap"      // DELIMITER //&
                          "Re"        // DELIMITER //&
                          "ncrit"     // DELIMITER //&
                          "spec"      // DELIMITER //&
                          "alpha"     // DELIMITER //&
                          "cl"        // DELIMITER //&
                          "cd"        // DELIMITER //&
                          "cl/cd"     // DELIMITER //&
                          "cm"        // DELIMITER //&
                          "xtr top"   // DELIMITER //&
                          "xtr bot"   // DELIMITER //&     
                          "dcl/cd"    // DELIMITER //&     
                          "at cl"          

end subroutine write_polar_header_csv


!------------------------------------------------------------------------------
! Write polar header in xfoil/xflr5 format to out_unit
!------------------------------------------------------------------------------
subroutine write_polar_header (out_unit, polar)

  type (polar_type), intent (in) :: polar
  integer,           intent (in) :: out_unit


! Example xflr5
!-
!Xoptfoil-JX Design_Polar 77
!
! Calculated polar for: JX FXrcn 15
!
! 1 1 Reynolds number fixed          Mach number fixed         
!
! xtrf =   1.000 (top)        1.000 (bottom)
! Mach =   0.000     Re =     0.400 e 6     Ncrit =   9.000
!
!-

  if (trim(polar%airfoil_name) == '') &
    call my_stop ('Xfoil polar to write has no name.')

  write (out_unit,'(A)') "Xoptfoil-JX" // " " // trim(polar%add_info)
  write (out_unit,*)
  write (out_unit,'(A)') " Calculated polar for: "//trim(polar%airfoil_name)
  write (out_unit,*)
  if (polar%re%type == 1 ) then 
    write (out_unit,'(A)') " 1 1 Reynolds number fixed          Mach number fixed"
  else
    write (out_unit,'(A)') " 2 2 Reynolds number ~ 1/sqrt(CL)   Mach number ~ 1/sqrt(CL)"
  end if 
  write (out_unit,*) 
  write (out_unit,'(A)') " xtrf =   1.000 (top)        1.000 (bottom)"
  write (out_unit,'(A,F7.3,5X,A,F9.3,A,5X,A,F7.3 )')                     &
                     " Mach = ",polar%ma%number,'Re = ',(polar%re%number/1.d6),' e 6','Ncrit = ',polar%ncrit
  write (out_unit,*)
  
end subroutine write_polar_header

!-----------------------------------------------------------------------------
! calculate number of op_points of a polar based on start, end, increment
!-----------------------------------------------------------------------------
function  get_n_op_points (polar)

    type (polar_type), intent (in) :: polar
    integer :: get_n_op_points
    double precision :: cur_value, end_value
  
    get_n_op_points = 0 
    cur_value       = polar%start_value
    end_value       = polar%end_value + 1.d-6    ! due to double prec compare
  
    do while (cur_value <= end_value)
      get_n_op_points = get_n_op_points + 1
      cur_value = cur_value +  polar%increment
    end do 
  
  end function get_n_op_points

!-----------------------------------------------------------------------------
! the smallest (absolute) value of operating points of polar 
!-----------------------------------------------------------------------------
  function  smallest_op_point (polar)

    type (polar_type), intent (in) :: polar
    double precision :: smallest_op_point
    double precision :: cur_value, end_value
  
    smallest_op_point = polar%start_value
    cur_value       = polar%start_value
    end_value       = polar%end_value + 1.d-6    ! due to double prec compare
  
    do while (cur_value <= end_value)
      if (abs (cur_value) < abs(smallest_op_point)) then
        smallest_op_point = cur_value
      end if 
      cur_value = cur_value +  polar%increment
    end do 
  
  end function smallest_op_point

  
!-----------------------------------------------------------------------------
! build filename for polar file in this special xflr5 format
!   example: T1_Re0.400_M0.00_N9.0.txt 
!-----------------------------------------------------------------------------
  function  build_filename (polar)

    type (polar_type), intent (in) :: polar
    character (25) :: build_filename
    character (5)  :: temp_String
  
    if(polar%re%type == 1) then 
        build_filename  = 'T1'
    else 
        build_filename  = 'T2'
    end if 

    build_filename  = trim(build_filename)  // '_Re'
    write (temp_String, '(F5.3)') polar%re%number / 1.d6
    build_filename  = trim(build_filename)  // trim(temp_String)

    build_filename  = trim(build_filename)  // '_M'
    write (temp_String, '(F4.2)') polar%ma%number
    build_filename  = trim(build_filename)  // trim(temp_String)

    build_filename  = trim(build_filename)  // '_N'
    if (polar%ncrit < 10d0) then 
      write (temp_String, '(F3.1)') polar%ncrit
    else
      write (temp_String, '(F3.0)') polar%ncrit
    end if

    build_filename  = trim(build_filename)  // trim(temp_String)

    build_filename  = trim(build_filename)  // '.txt'

  end function build_filename


!------------------------------------------------------------------------------
! Sort op points of a polar ascending based on alpha or cl (spec-cl) 
! - Removes all not converged op points 
!------------------------------------------------------------------------------
subroutine sort_op_points (show_details, spec_cl, op_points_result, op_points_sorted, has_warned)

  use xfoil_driver,       only : op_point_result_type

  logical,              intent(in)  :: show_details, spec_cl
  type (op_point_result_type), dimension (:), intent (in)  :: op_points_result
  type (op_point_result_type), dimension (:), intent (inout), allocatable :: op_points_sorted
  logical,              intent(inout)  :: has_warned

  type (op_point_result_type)       :: tmp_op
  integer              :: i, j, nconverged
  logical              :: bubble , has_bubbled

  has_bubbled = .true.
  nconverged = 0

! Build initial result array without not converged op points

  do i=1, size(op_points_result) 
    if (op_points_result(i)%converged) nconverged = nconverged + 1
  end do 

  if (nconverged == 0) then 
    return 
  end if 
  if (allocated(op_points_sorted))  deallocate (op_points_sorted)
  allocate (op_points_sorted(nconverged))

  j = 0 
  do i=1, size(op_points_result)
    if (op_points_result(i)%converged) then 
      j = j + 1
      op_points_sorted(j) = op_points_result(i)
    else
      if (show_details) call print_failed_op_value (op_points_result(i), spec_cl, has_warned)
    end if 
  end do 

! Bubble sort of op_points 

  do while (has_bubbled)
    has_bubbled = .false. 
    do i = 1, (size(op_points_sorted) - 1) 
      bubble = .false.
      if (spec_cl) then 
        if (op_points_sorted(i)%cl > op_points_sorted(i+1)%cl) bubble = .true. 
      else
        if (op_points_sorted(i)%alpha > op_points_sorted(i+1)%alpha) bubble = .true. 
      end if 
      if (bubble) then
        tmp_op = op_points_sorted(i)
        op_points_sorted(i)   = op_points_sorted (i+1) 
        op_points_sorted(i+1) = tmp_op
        has_bubbled = .true. 
      end if  
    end do 
  end do 
    
end subroutine sort_op_points

!------------------------------------------------------------------------------
end module polar_operations