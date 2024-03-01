! MIT License
! Copyright (c) 2020-2024 Jochen Guenzel 

module polar_operations

  ! create and write xfoil based polars

  use os_util
  use print_util
  
  use xfoil_driver,       only : re_type, op_point_spec_type
  use xfoil_driver,       only : op_point_result_type

  implicit none
  private

  PUBLIC :: initialize_polars
  PUBLIC :: generate_polar_set
  PUBLIC :: set_polar_info
  public :: polar_type

  type polar_type
    character(:), allocatable       :: file_name       ! Name of polar file name 
    character(:), allocatable       :: add_info        ! additional info string in polar file 
    type(re_type)                   :: re              ! Re number of this polar (re*sqrt(cl) if Type2)
    type(re_type)                   :: ma              ! Ma number of this polar (mach*sqrt(cl) if Type2)
    double precision                :: ncrit           ! ncrit of polar
    logical                         :: spec_cl         ! base value of polar either cl or alpha
    double precision                :: start_value     ! polar starting from ...
    double precision                :: end_value       ! ... to end value ...
    double precision                :: increment       ! ... incremented by 
    type(op_point_spec_type), allocatable   :: op_points_spec (:) ! array with specified op_points
    type(op_point_result_type), allocatable :: op_points (:)      ! array with all calculated op_points
  end type polar_type

  character(1), parameter    :: DELIMITER = ','

contains

  
  subroutine generate_polar_files (show_details, foil, xfoil_options, polars)

    !----------------------------------------------------------------------------
    !! ** not tested **
    !! Generate and write to file all 'npolars' 'polars' for an airfoil
    !!    Each polar will be written in a single file in xfoil text format
    !!    in the subdirectory 'foil%name_polars' 
    !!    The name of the file is aligned to xflr5 polar file naming
    !----------------------------------------------------------------------------

    use airfoil_base,       only : airfoil_type, panel_options_type
    use os_util,            only : make_directory
    use xfoil_driver,       only : xfoil_options_type
    use xfoil_driver,       only : op_point_result_type, run_op_points 
    use xfoil_driver,       only : flap_spec_type

    type (airfoil_type), intent (in)  :: foil
    logical, intent(in)               :: show_details
    type (xfoil_options_type), intent(in) :: xfoil_options
    type (polar_type), intent(in)         :: polars (:) 


    type (xfoil_options_type)         :: local_xfoil_options
    double precision, allocatable     :: flap_angle (:)
    type(flap_spec_type)              :: flap_spec               ! dummy - no flaps used
    type(op_point_result_type), allocatable :: op_points_result (:)
    integer                           :: i, nop_points
    character (:), allocatable        :: polar_subdirectory, polar_label, polar_path

    flap_spec%use_flap  = .false. 
    local_xfoil_options = xfoil_options
    local_xfoil_options%exit_if_unconverged = .false.  ! we need all op points
    local_xfoil_options%detect_outlier      = .false.  ! makes no sense for polar calcualtion being excuted only once
    local_xfoil_options%show_details = show_details

    ! Create subdir for polar files if not exist

    polar_subdirectory = foil%name//'_polars'
    polar_path         = path_join (polar_subdirectory, polars(i)%file_name)
    call make_directory (polar_subdirectory, .true.) ! preserve existing

    ! flaps angle will be 0

    nop_points   = size(polars(1)%op_points_spec)
    allocate (flap_angle(nop_points))
    flap_angle (:)    = 0.d0 

    ! calc and write all polars

    do i = 1, size(polars)

      if (allocated(op_points_result))  deallocate (op_points_result)
    
      if (show_details) then 
        write (polar_label,'(A, I7)') 'Re=',  int(polars(i)%re%number)
        call print_text ('- Generating polar ' // trim(polar_label) // '  ')
      end if 

      call run_op_points (foil, local_xfoil_options,        &
                          flap_spec, flap_angle, &
                          polars(i)%op_points_spec, op_points_result)
    

      if (show_details) then 
        call print_colored (COLOR_NOTE,' - Writing polar ' // trim(polar_label) &
                                      //' to '//trim(polar_subdirectory) //'   ')
      end if 

      open(unit=13, file= polar_path, status='replace')
      call write_polar_header (13, foil%name, polars(i))
      call write_polar_data   (show_details, 13, polars(i), op_points_result)
      close (13)

    end do 

  end subroutine generate_polar_files



  subroutine generate_polar_set (show_details, csv_format, foil, &
                                flap_spec, flap_angle, xfoil_options, &
                                polars)

    !----------------------------------------------------------------------------
    !! * multi threaded version of generate_polar_files - only for 'worker' * 
    !!
    !! Generate and write to file all 'npolars' 'polars' for an airfoil
    !!      Each polar will be written in a single file in xfoil text format
    !!      in 'subdirectory'
    !!
    !!      The name of the file is aligned to xflr5 polar file naming
    !----------------------------------------------------------------------------

    use airfoil_base,       only : airfoil_type, airfoil_name_flapped
    use os_util,            only : make_directory
    use xfoil_driver,       only : xfoil_options_type
    use xfoil_driver,       only : op_point_result_type, run_op_points 
    use xfoil_driver,       only : xfoil_init, xfoil_cleanup
    use xfoil_driver,       only : flap_spec_type

    type (airfoil_type), intent (in)      :: foil
    logical, intent(in)                   :: show_details
    logical, intent(in)                   :: csv_format
    type(flap_spec_type), intent(in)      :: flap_spec  
    double precision, intent(in)          :: flap_angle (:) 
    type (xfoil_options_type), intent(in) :: xfoil_options
    type (polar_type), intent(in)         :: polars (:) 


    type(xfoil_options_type)          :: local_xfoil_options
    double precision, allocatable     :: flap_angle_op_points (:)
    type(op_point_result_type), allocatable :: op_points_result (:)
    type(op_point_result_type), allocatable :: results (:,:,:)
    integer                           :: i, j, nflap_angles, npolars, nop_points
    character (:), allocatable        :: polar_subdirectory, polar_path, foil_name
    logical                           :: exist

    nflap_angles = size(flap_angle)
    npolars      = size(polars) 
    nop_points   = size(polars(1)%op_points_spec)

    if (npolars * nflap_angles > 2 ) then
      call print_action ("A total of "//stri(npolars * nflap_angles)//" polars with each "//&
                        stri(nop_points)// " operating points will be generated ...")
      print *
    end if 

    ! the master result matrix with op_results for all flaps and polars 

    allocate (results(nflap_angles, npolars, nop_points))

    ! set xfoil options for polar generation 

    local_xfoil_options = xfoil_options
    local_xfoil_options%exit_if_unconverged = .false.   ! we need all op points
    local_xfoil_options%detect_outlier      = .false.   ! makes no sense for polar calcualtion being excuted only once
    local_xfoil_options%maxit               = 100       ! increase default value of xfoil iterations

    if (npolars == 1 .and. nflap_angles == 1) then
      local_xfoil_options%show_details = show_details
    else
      local_xfoil_options%show_details = .false.        ! multi thread would mix up output
    end if 

    ! init xfoil mutli threaded 
    call xfoil_cleanup()                 ! deallocate xfoil if already allocated (single)  

    !$omp parallel default(shared)
    call xfoil_init()                    ! Allocate private memory for xfoil on each thread 
    !$omp end parallel      

    ! Multi threaded polars   
    ! 
    !$omp parallel do schedule(DYNAMIC) collapse(2) private(i, j, flap_angle_op_points, op_points_result) 

    do j = 1, nflap_angles
      do i = 1, npolars

        if (allocated(flap_angle_op_points)) deallocate (flap_angle_op_points)
        allocate (flap_angle_op_points(nop_points))
        flap_angle_op_points (:) = flap_angle(j)

        call print_action ('Generating polar '// get_polar_label (flap_angle(j), polars(i)))      

        call run_op_points (foil, local_xfoil_options, flap_spec, flap_angle_op_points, &
                            polars(i)%op_points_spec, op_points_result) 

        results (j,i,1:size(op_points_result)) = op_points_result

      end do 
    end do 
    !$omp end parallel do

    !$omp parallel default(shared)
    call xfoil_cleanup()
    !$omp end parallel      

    ! write out result matrix 

    print * 
    
    do j = 1, nflap_angles
      do i = 1, npolars

        ! Create subdir for polar files if not exist

        foil_name          = airfoil_name_flapped (foil, flap_angle(j) )
        polar_subdirectory = foil_name//'_polars'
        polar_path         = path_join (polar_subdirectory, polars(i)%file_name)
        call make_directory (polar_subdirectory, .true.) ! preserve existing

        ! write data 

        call print_action ('Writing polar '//get_polar_label (flap_angle(j), polars(i))//' to',&
                            polar_subdirectory //' ', no_crlf=.true.)      

        op_points_result = results (j,i,1:nop_points)
        if (.not. csv_format) then 

          open(unit=13, file= trim(polar_path), status='replace')
          call write_polar_header (13, foil_name, polars(i))
          call write_polar_data   (show_details, 13, polars(i), op_points_result)

        else 

          inquire(file=trim(polar_path), exist=exist)
          if (exist) then       ! append other polars to the file 
            open(unit=13, file= trim(polar_path), status='old', position='append')
          else                  ! csv Header only for new file at the beginning
            open(unit=13, file= trim(polar_path), status='new')
            call write_polar_header_csv (13)
          end if
          call write_polar_data_csv (show_details, 13, foil_name, flap_angle(j), polars(i), op_points_result)
        end if 
        close (13)

      end do 
    end do 

  end subroutine generate_polar_set



  subroutine initialize_polars  (spec_cl, op_point_range, type_of_polar, ncrit, polar_reynolds, &
                                foil_name, csv_format, polars) 

    !----------------------------------------------------------------------------
    !! Init polar data structure in this module
    !!   - re_default for polar definitions with no Reynolds
    !!   - nrit for the polar xfoil calculation
    !!   - name of foil
    !! Returns:  array of polars 
    !----------------------------------------------------------------------------

    use xfoil_driver,       only : xfoil_options_type
    ! use input_read,       only : namelist_check

    logical,          intent(in) :: spec_cl 
    integer,          intent(in) :: type_of_polar                  ! 1 or 2 
    double precision, intent(in) :: ncrit 
    double precision, intent(in) :: polar_reynolds (:)             ! 40000, 70000, 100000
    double precision, intent(in) :: op_point_range (:)             ! -1.0, 10.0, 0.5
    logical,          intent(in) :: csv_format         
    character(*),     intent(in) :: foil_name

    type (polar_type), allocatable, intent(out) :: polars (:) 
          
    double precision    :: cur_value
    integer             :: i, j, npolars, nop_points

    npolars = size(polar_reynolds)

    ! Init polar definitions with input 

    allocate (polars(npolars))

    do i = 1, npolars

      polars(i)%add_info        = ''
      polars(i)%spec_cl         = spec_cl
      polars(i)%start_value     = op_point_range (1)
      polars(i)%end_value       = op_point_range (2)
      polars(i)%increment       = op_point_range (3)
      polars(i)%ma%number       = 0.0d0                   ! currently not supported
      polars(i)%ma%type         = 1                       ! currently not supported
      polars(i)%re%number       = polar_reynolds(i)
      polars(i)%re%type         = type_of_polar
      polars(i)%ncrit           = ncrit

      if (csv_format) then
        polars(i)%file_name = trim(foil_name)//'.csv'
      else
      ! build this special xflr5 filename  T1_Re0.400_M0.00_N9.0.txt 
        polars(i)%file_name = get_polar_filename (polars(i))
      end if

    end do

    ! init op points spec from the range specification 

    do i = 1, npolars

      ! calc number of op_points 

      nop_points = get_nop_points (polars(i)) 
      if (nop_points >= 0) then
        if (allocated(polars(i)%op_points))      deallocate (polars(i)%op_points)
        if (allocated(polars(i)%op_points_spec)) deallocate (polars(i)%op_points_spec)
        ! only spec - op_points will be allocated in xfoil driver ...
        allocate (polars(i)%op_points_spec(nop_points))
      else
        call my_stop ("No valid value boundaries for polar")
      endif

      ! init op data points of polar
      !
      ! if polar is running from eg -5 to +10, split the polar in
      !   0     ... -5
      !   (0+x) ... + 10
      ! to ensure xfoil convergence (starting with a high negative value is critical)

      j = 0 

      if (polars(i)%start_value < 0d0) then 

        ! go downward from smallest-1 to start value 

        if (polars(i)%end_value > 0d0) then 
          cur_value =  smallest_op_point (polars(i)) - polars(i)%increment
        else 
          cur_value =  smallest_op_point (polars(i)) 
        end if 

        do while (cur_value >= polars(i)%start_value)
          j = j + 1
          polars(i)%op_points_spec(j)%value    = cur_value
          polars(i)%op_points_spec(j)%spec_cl  = polars(i)%spec_cl
          polars(i)%op_points_spec(j)%re       = polars(i)%re
          polars(i)%op_points_spec(j)%ma       = polars(i)%ma
          polars(i)%op_points_spec(j)%ncrit    = polars(i)%ncrit
          cur_value = cur_value - polars(i)%increment
        end do       
      end if 

      if (polars(i)%end_value > 0d0) then 

        ! go upward from smallest to end value 

        cur_value = smallest_op_point (polars(i))
        do while (cur_value <= polars(i)%end_value + 1.d-6)
          j = j + 1
          polars(i)%op_points_spec(j)%value    = cur_value
          polars(i)%op_points_spec(j)%spec_cl  = polars(i)%spec_cl
          polars(i)%op_points_spec(j)%re       = polars(i)%re
          polars(i)%op_points_spec(j)%ma       = polars(i)%ma
          polars(i)%op_points_spec(j)%ncrit    = polars(i)%ncrit
          cur_value = cur_value + polars(i)%increment
        end do       
      end if 

    end do

  end subroutine 



  subroutine set_polar_info (file_name, add_info, polars)

    !------------------------------------------------------------------------------
    !! Set info for polar filename and additional info like "Design 123"
    !------------------------------------------------------------------------------

    type (polar_type), allocatable, intent(inout) :: polars (:) 
    character (*), intent(in) :: file_name, add_info

    integer     :: i

    if (size(polars) > 0) then 
      do i = 1, size(polars)
        if (file_name /= '') polars(i)%file_name = file_name
        if (add_info  /= '') polars(i)%add_info  = add_info
      end do 
    else
      call my_stop("No polar initialized for generate_polar.")
    end if 

  end subroutine 



  function get_polar_label (flap_angle, polar) result (aLabel)

    !! returns a nice label for polar 

    double precision, intent(in)    :: flap_angle
    type (polar_type), intent(in)   :: polar 
    character(:), allocatable       :: aLabel

    aLabel = get_polar_name (polar) // ' '

    if (flap_angle /= 0.0) then 
      aLabel = aLabel // ' of flap angle ' // strf('(F4.1)',flap_angle, fix=.true.) // ' '
    endif 

    return 



  end function 



  subroutine write_polar_data (show_details, out_unit, polar, op_points_result)

    !------------------------------------------------------------------------------
    !! Write polar data in xfoil format to out_unit
    !------------------------------------------------------------------------------

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
    if (has_warned) then
      call print_colored (COLOR_WARNING," skipped ")
      call print_colored (COLOR_NOTE,"as not converged")
    end if 
    print *

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



  subroutine write_polar_data_csv (show_details, out_unit, foil_name, flap_angle, polar, op_points_result)

    !------------------------------------------------------------------------------
    !! Write polar data of foil in csv format to out_unit
    !------------------------------------------------------------------------------

    use xfoil_driver,       only : op_point_result_type

    logical,              intent(in)  :: show_details
    integer,              intent(in)  :: out_unit
    character (*),        intent(in)  :: foil_name
    double precision,     intent(in)  :: flap_angle
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
      call print_colored (COLOR_WARNING," skipped ")
      call print_colored (COLOR_NOTE,"as not converged")
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
                    foil_name,                DELIMITER, & 
                    flap_angle,               DELIMITER, & 
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

    print * 
    
  end subroutine write_polar_data_csv



  subroutine print_failed_op_value (op, spec_cl, has_warned)

    !------------------------------------------------------------------------------
    !! Print the value of failed op during writing output data 
    !------------------------------------------------------------------------------

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



  subroutine write_polar_header_csv (out_unit)

    !------------------------------------------------------------------------------
    !! Write polar header csv d
    !------------------------------------------------------------------------------

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



  subroutine write_polar_header (out_unit, foil_name, polar)

    !------------------------------------------------------------------------------
    !! Write polar header in xfoil/xflr5 format to out_unit
    !------------------------------------------------------------------------------

    type (polar_type), intent(in) :: polar
    integer,           intent(in) :: out_unit
    character (*),     intent(in) :: foil_name

    ! Example xflr5
    !-
    !Xoptfoil2 Design_Polar 77
    !
    ! Calculated polar for: JX FXrcn 15
    !
    ! 1 1 Reynolds number fixed          Mach number fixed         
    !
    ! xtrf =   1.000 (top)        1.000 (bottom)
    ! Mach =   0.000     Re =     0.400 e 6     Ncrit =   9.000
    !
    !-

    if (foil_name == '') &
      call my_stop ('Xfoil polar to write has no name.')

    write (out_unit,'(A)') "Xoptfoil2" // " " // polar%add_info
    write (out_unit,*)
    write (out_unit,'(A)') " Calculated polar for: "//foil_name
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



  function  get_nop_points (polar)

    !-----------------------------------------------------------------------------
    !! calculate number of op_points of a polar based on start, end, increment
    !-----------------------------------------------------------------------------

    type (polar_type), intent (in) :: polar
    integer :: get_nop_points
    double precision :: cur_value, end_value

    get_nop_points = 0 
    cur_value       = polar%start_value
    end_value       = polar%end_value + 1.d-6    ! due to double prec compare

    do while (cur_value <= end_value)
      get_nop_points = get_nop_points + 1
      cur_value = cur_value +  polar%increment
    end do 

  end function 



  function  smallest_op_point (polar)

    !-----------------------------------------------------------------------------
    !! the smallest (absolute) value of operating points of polar 
    !-----------------------------------------------------------------------------

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


  
  function  get_polar_name (polar) result (polar_name)

    !-----------------------------------------------------------------------------
    !! returns polar name  like  T1_Re0.400_M0.00_N9.0
    !-----------------------------------------------------------------------------

    type (polar_type), intent (in) :: polar
    character (:), allocatable  :: polar_name
    character (25)              :: build_name
    character (5)               :: temp_String
  
    if(polar%re%type == 1) then 
        build_name  = 'T1'
    else 
        build_name  = 'T2'
    end if 

    build_name  = trim(build_name)  // '_Re'
    write (temp_String, '(F5.3)') polar%re%number / 1.d6
    build_name  = trim(build_name)  // trim(temp_String)

    build_name  = trim(build_name)  // '_M'
    write (temp_String, '(F4.2)') polar%ma%number
    build_name  = trim(build_name)  // trim(temp_String)

    build_name  = trim(build_name)  // '_N'
    if (polar%ncrit < 10d0) then 
      write (temp_String, '(F3.1)') polar%ncrit
    else
      write (temp_String, '(F3.0)') polar%ncrit
    end if

    build_name  = trim(build_name)  // trim(temp_String)

    polar_name = trim(build_name)

  end function 


  
  function  get_polar_filename (polar) result (filename)

    !-----------------------------------------------------------------------------
    !! returns filename for polar file in this special xfoil format
    !!    example: T1_Re0.400_M0.00_N9.0.txt 
    !-----------------------------------------------------------------------------
    type (polar_type), intent (in) :: polar
    character (:), allocatable  :: filename

    filename = get_polar_name (polar)// ".txt"

  end function 



  subroutine sort_op_points (show_details, spec_cl, op_points_result, op_points_sorted, has_warned)

    !------------------------------------------------------------------------------
    !! Sort op points of a polar ascending based on alpha or cl (spec-cl) 
    ! - Removes all not converged op points 
    !------------------------------------------------------------------------------

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

end module