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

  public :: initialize_polars
  public :: generate_polar_set
  public :: polar_type

  type polar_type
    character(:), allocatable       :: file_name       ! Name of polar file name 
    character(:), allocatable       :: add_info        ! additional info string in polar file 
    type(re_type)                   :: re              ! Re number of this polar (re*sqrt(cl) if Type2)
    type(re_type)                   :: ma              ! Ma number of this polar (mach*sqrt(cl) if Type2)
    double precision                :: ncrit           ! ncrit of polar
    logical                         :: auto_range      ! op point range will be automatically determined
    logical                         :: spec_cl         ! base value of polar either cl or alpha
    double precision                :: start_value     ! polar starting from ...
    double precision                :: end_value       ! ... to end value ...
    double precision                :: increment       ! ... incremented by 
    type(op_point_spec_type), allocatable   :: op_points_spec (:) ! array with specified op_points
    type(op_point_result_type), allocatable :: op_points (:)      ! array with all calculated op_points
  end type polar_type

  character(1), parameter    :: DELIMITER = ';'

contains


  subroutine generate_polar_set (auto_range, output_prefix, csv_format, foil, &
                                flap_spec, flap_angle, xfoil_options, polars, splitted)

    !----------------------------------------------------------------------------
    !! * multi threaded version - only for 'worker' * 
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
    use xfoil_driver,       only : xfoil_init, xfoil_cleanup, xfoil_stats_print
    use xfoil_driver,       only : flap_spec_type
    use eval_out,           only : write_airfoil_flapped


    logical, intent(in)                   :: auto_range     ! cl max will be auto detected
    character (*), intent(in), optional   :: output_prefix
    type (airfoil_type), intent (in)      :: foil
    logical, intent(in)                   :: csv_format
    type(flap_spec_type), intent(in)      :: flap_spec  
    double precision, intent(in)          :: flap_angle (:) 
    type (xfoil_options_type), intent(in) :: xfoil_options
    type (polar_type), intent(in)         :: polars (:) 
    logical, intent(in)                   :: splitted


    type (polar_type)                 :: polar 
    type(xfoil_options_type)          :: local_xfoil_options
    double precision, allocatable     :: flap_angle_op_points (:)
    type(op_point_result_type), allocatable :: op_points_result (:)
    type(op_point_result_type), allocatable :: results (:,:,:)
    integer                           :: i, j, nflap_angles, npolars, nop_points, max_nop_points
    integer                           :: ngenerate, nfinal, ip
    character (:), allocatable        :: polar_subdirectory, polar_path, base_name, auto_text
    logical                           :: exist

    nflap_angles   = size(flap_angle)
    npolars        = size(polars) 
    ngenerate      = npolars * nflap_angles
    max_nop_points = max (size(polars(1)%op_points_spec), size(polars(2)%op_points_spec))

    if (ngenerate > 2 ) then
      if (auto_range) then 
        auto_text = "with auto_range "
      else 
        auto_text = ""
      end if 
      if (splitted) then 
        call print_action ("A total of "//stri(ngenerate)//" splitted polars "// auto_text//"will be generated ...")
      else
        call print_action ("A total of "//stri(ngenerate)//" polars with max "//&
                            stri(max_nop_points)// " operating points will be generated ...")
      end if
      print *
    end if 

    ! the master result matrix with op_results for all flaps and polars 

    allocate (results(nflap_angles, npolars, max_nop_points))

    ! set xfoil options for polar generation 

    local_xfoil_options = xfoil_options
    local_xfoil_options%show_details        = .false.   ! multi thread would mix up output
    local_xfoil_options%exit_if_unconverged = .false.   ! we need all op points
    local_xfoil_options%detect_outlier      = .false.   ! makes no sense for polar calculation being executed only once
    local_xfoil_options%maxit               = 100       ! increase default value of xfoil iterations
    local_xfoil_options%fix_unconverged     = .true. 
    if (auto_range) then 
      local_xfoil_options%exit_if_clmax       = .true.  ! auto detect cl max 
    else 
      local_xfoil_options%exit_if_clmax       = .false. ! normal mode  
    end if 

    ! init xfoil mutli threaded 
    call xfoil_cleanup()                 ! deallocate xfoil if already allocated (single)  

    !$omp parallel default(shared)
    call xfoil_init()                    ! Allocate private memory for xfoil on each thread 
    !$omp end parallel      

    ! Multi threaded polars   
    ! 
    !$omp parallel do schedule(DYNAMIC) collapse(2) private(i, flap_angle_op_points, op_points_result) 

    do j = 1, nflap_angles
      do i = 1, npolars

        nop_points = size(polars(i)%op_points_spec)
        if (allocated(flap_angle_op_points)) deallocate (flap_angle_op_points)
        allocate (flap_angle_op_points(nop_points))
        flap_angle_op_points (:) = flap_angle(j)

        !$omp critical  
        call print_action ('Generating polar '// get_polar_label (flap_angle(j), polars(i)))      
        !$omp end critical  

        call run_op_points (foil, local_xfoil_options, flap_spec, flap_angle_op_points, &
                            polars(i)%op_points_spec, op_points_result) 


        !$omp critical  
        results (j,i,1:size(op_points_result)) = op_points_result
        call print_action ('Finished '// get_polar_label (flap_angle(j), polars(i)))      
        !$omp end critical  

      end do 
    end do 
    !$omp end parallel do

    !$omp parallel default(shared)
    call xfoil_cleanup()
    !$omp end parallel      


    ! print xfoil statistics 

    print * 
    call xfoil_stats_print (5)

    ! write out result matrix 

    print * 

    if (splitted) then 
      nfinal = npolars / 2
    else
      nfinal = npolars
    end if 
        
    do j = 1, nflap_angles

      ! Create subdir for polar files if not exist
         
      base_name          = airfoil_name_flapped (foil, flap_angle(j), base_name=output_prefix )
      polar_subdirectory = base_name//'_polars'
      call make_directory (polar_subdirectory, .true.) ! preserve existing

      do i = 1, nfinal

        ! join splitted polar into one final result polar 

        if (splitted) then 
          ip = (i-1) * 2 + 1                          ! index of splitted polars 
          op_points_result = [trim_result(results (j,ip,:)), trim_result(results (j,ip+1,:))]   ! concatenate array
        else
          ip = i
          op_points_result = trim_result (results (j,ip,:))
        end if 

        polar%file_name = polars(ip)%file_name
        polar%add_info  = "" 
        polar%re        = polars(ip)%re
        polar%ma        = polars(ip)%ma
        polar%ncrit     = polars(ip)%ncrit
        polar%spec_cl   = polars(ip)%spec_cl

        ! write polar data to file 


        if (.not. csv_format) then 

          call print_action ('Writing polar '//get_polar_label (flap_angle(j), polar)//' to',&
                              polar_subdirectory //' ', no_crlf=.true.)      
          polar_path = path_join (polar_subdirectory, polar%file_name)
          open(unit=13, file= trim(polar_path), status='replace')
          call write_polar_header (13, foil%name, polar)
          call write_polar_data   (.false., 13, polar, op_points_result)

        else 

          polar_path = polar%file_name
          inquire(file=trim(polar_path), exist=exist)
          if (exist) then       ! append other polars to the file 
            call print_action ('Appending polar '//get_polar_label (flap_angle(j), polar)//' to',&
                                polar_path //' ', no_crlf=.true.)      
            open(unit=13, file= trim(polar_path), status='old', position='append')
          else                  ! csv Header only for new file at the beginning
            call print_action ('Writing   polar '//get_polar_label (flap_angle(j), polar)//' to',&
                                polar_path //' ', no_crlf=.true.)      
            open(unit=13, file= trim(polar_path), status='new')
            call write_polar_header_csv (13)
          end if
          call write_polar_data_csv (.false., 13, foil%name, flap_angle(j), polar, op_points_result)
        end if 
        close (13)

      end do 
    end do 

    ! Now set flap to all requested angles and write airfoils 

    if (nflap_angles > 1 .or. flap_angle(1) /= 0d0) then
      print *
      call write_airfoil_flapped (foil, flap_spec, flap_angle, .true.)
    end if 

  end subroutine generate_polar_set



  subroutine initialize_polars  (auto_range, spec_cl_in, op_point_range, type_of_polar, ncrit, &
                                 polar_reynolds, polar_mach, &
                                 output_prefix, csv_format, polars, splitted) 

    !----------------------------------------------------------------------------
    !! Init polar data structure in this module
    !!   - re_default for polar definitions with no Reynolds
    !!   - nrit for the polar xfoil calculation
    !!   - name of foil
    !! Returns:  array of polars 
    !----------------------------------------------------------------------------

    use xfoil_driver,       only : xfoil_options_type

    logical,          intent(in) :: auto_range                     ! op point range will be automatically determined 
    logical,          intent(in) :: spec_cl_in 
    integer,          intent(in) :: type_of_polar                  ! 1 or 2 
    double precision, intent(in) :: ncrit 
    double precision, intent(in) :: polar_reynolds (:)             ! 40000, 70000, 100000
    double precision, intent(in) :: polar_mach (:)                 ! 0.0, 0.1, 0.0 
    double precision, intent(in) :: op_point_range (:)             ! -1.0, 10.0, 0.5
    logical,          intent(in) :: csv_format         
    character(*),     intent(in) :: output_prefix

    type (polar_type), allocatable, intent(out) :: polars (:) 
    logical,          intent(out) :: splitted 
          
    double precision    :: cur_value, start_value, end_value, increment
    integer             :: i, j, npolars, nop_points, nreynolds, ire
    logical             :: spec_cl 

    nreynolds = size(polar_reynolds)

    ! if auto_range determine start, end, increment 

    if (.not. auto_range) then 
      spec_cl     = spec_cl_in
      start_value = op_point_range (1)
      end_value   = op_point_range (2)
      increment   = op_point_range (3)
    else 
      if (type_of_polar == 1) then 
        spec_cl     = .false.
        start_value = -20d0                           ! high value for auto_detect of start / end 
        end_value   =  20d0
        increment   = 0.2d0
      else
        spec_cl     = .true.
        start_value = 0.02d0                           ! high value for auto_detect of start / end 
        end_value   =  5d0
        increment   = 0.02d0
      end if 
    end if 

    ! if op point range is from '-' to '+' the polar will be splitted into 2 
    ! starting from smallest value - mostly 0.0  
    !   to ensure xfoil convergence (starting with a high negative value is critical) 
    !   to speed-up (multi-threating)

    if (start_value < 0d0 .and. end_value > 0d0) then 
      splitted = .true.
      npolars = nreynolds * 2 
    else 
      splitted = .false.
      npolars = nreynolds  
    end if 
    allocate (polars(npolars))

    ! Init polar definitions with input 

    polars%auto_range      = auto_range
    polars%spec_cl         = spec_cl
    polars%ma%type         = 1                       ! only Type 1 supported 
    polars%re%type         = type_of_polar
    polars%ncrit           = ncrit

    if (splitted) then 
      do ire = 1, nreynolds
        i = (ire-1) * 2 + 1
        polars(i)%add_info        = 'down'
        polars(i)%start_value     = smallest_abs_value (start_value, end_value, increment)
        polars(i)%end_value       = start_value 
        polars(i)%increment       = - increment 
        polars(i)%ma%number       = polar_mach (ire)   
        polars(i)%re%number       = polar_reynolds(ire)
        i = i + 1
        polars(i)%add_info        = 'up'
        polars(i)%start_value     = smallest_abs_value (start_value, end_value, increment) + increment
        polars(i)%end_value       = end_value 
        polars(i)%increment       =  increment 
        polars(i)%ma%number       = polar_mach (ire)   
        polars(i)%re%number       = polar_reynolds(ire)
      end do 
    else 
      do ire = 1, nreynolds
        i = ire
        polars(i)%add_info        = ''
        polars(i)%start_value     = start_value
        polars(i)%end_value       = end_value 
        polars(i)%increment       = increment 
        polars(i)%ma%number       = polar_mach (ire)   
        polars(i)%re%number       = polar_reynolds(ire)
      end do 
    end if 

    ! build this special xflr5 filename  T1_Re0.400_M0.00_N9.0.txt 

    do i = 1, npolars
      if (csv_format) then
        polars(i)%file_name = trim(output_prefix)//'.csv'
      else
        polars(i)%file_name = get_polar_filename (polars(i))
      end if
    end do

    ! init op points spec from the range specification 

    do i = 1, npolars

      ! calc number of op_points 

      nop_points = get_nop_points (polars(i)) 
      if (nop_points > 0) then
        if (allocated(polars(i)%op_points))      deallocate (polars(i)%op_points)
        if (allocated(polars(i)%op_points_spec)) deallocate (polars(i)%op_points_spec)
        ! only spec - op_points will be allocated in xfoil driver ...
        allocate (polars(i)%op_points_spec(nop_points))
      else
        call my_stop ("No valid value boundaries for polar")
      endif

      ! init op data points of polar

      cur_value =  polars(i)%start_value
      do j = 1, nop_points
        polars(i)%op_points_spec(j)%value    = cur_value
        polars(i)%op_points_spec(j)%spec_cl  = polars(i)%spec_cl
        polars(i)%op_points_spec(j)%re       = polars(i)%re
        polars(i)%op_points_spec(j)%ma       = polars(i)%ma
        polars(i)%op_points_spec(j)%ncrit    = polars(i)%ncrit
        cur_value = cur_value + polars(i)%increment
      end do       

    end do

  end subroutine 


  function get_polar_label (flap_angle, polar) result (aLabel)

    !! returns a nice label for polar 

    double precision, intent(in)    :: flap_angle
    type (polar_type), intent(in)   :: polar 
    character(:), allocatable       :: aLabel
    character (4)                   :: up_down

    aLabel = get_polar_name (polar) // ' '

    if (polar%add_info /= '') then 
      up_down = polar%add_info
      aLabel = aLabel // ' ' // up_down
    endif 

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
    type (op_point_result_type)       :: op
    integer              :: i 
    character (5  )      :: spec
    logical              :: has_warned

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

      write (out_unit,&
      '(A,A, F5.1, A, F8.0,A, F7.3,A, F4.1,A, A,A, F7.2,A, F7.3,A, F7.5,A, F6.2,A, F7.4,A, F6.3,A, F6.3,A, F8.5,A, F7.3)') &
                    foil_name,                DELIMITER, & 
                    flap_angle,               DELIMITER, & 
                    polar%re%number,          DELIMITER, & 
                    polar%ma%number,          DELIMITER, & 
                    polar%ncrit,              DELIMITER, & 
                    trim(spec),               DELIMITER, &      
                    op%alpha,                 DELIMITER, &      
                    op%cl,                    DELIMITER, &      
                    op%cd,                    DELIMITER, &      
                    op%cl / op%cd,            DELIMITER, &      
                    op%cm,                    DELIMITER, &      
                    op%xtrt,                  DELIMITER, &      
                    op%xtrb,                  DELIMITER
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
                            "Mach"      // DELIMITER //&
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

    use commons,     only : PGM_NAME

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

    write (out_unit,'(A)') PGM_NAME// " " // polar%add_info
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
    end_value       = polar%end_value     

    do while (abs(cur_value) <= (abs(end_value) + 1.d-6))     ! due to double prec compare
      get_nop_points = get_nop_points + 1
      cur_value = cur_value +  polar%increment
    end do 

  end function 

  

  function  smallest_abs_value (start_value, end_value, increment) result (smallest) 

    !-----------------------------------------------------------------------------
    !! the smallest (absolute) value of operating points of polar 
    !-----------------------------------------------------------------------------

    double precision, intent (in) :: start_value, end_value, increment
    double precision :: smallest, cur, end 
  
    smallest = start_value
    cur  = start_value
    end  = end_value + 1.d-6    ! due to double prec compare
  
    do while (cur <= end)
      if (abs (cur) < abs(smallest)) then
        smallest = cur
      end if 
      cur = cur + increment
    end do 
  
  end function 


  function trim_result (op_points) result (trimmed_op_points)

    !-----------------------------------------------------------------------------
    !! removes not valid op points at the end of array op_points 
    !-----------------------------------------------------------------------------
    
    type (op_point_result_type), intent(in)   :: op_points (:) 

    type (op_point_result_type), allocatable  :: trimmed_op_points (:)
    type (op_point_result_type)               :: op
    integer   :: i 
    
    do i = size(op_points), 1, -1 
      op = op_points (i) 
      if (op%converged .and. op%cl /= 0d0 .and. op%cd /= 0d0) exit 
    end do 
    
    trimmed_op_points = op_points (1:i)
    
  end function 

  
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