! MIT License
! Copyright (c) 2024 Jochen Guenzel

!
! utility functions for automated tests 
!

module test_util

  !-------------------------------------------------------------------------
  ! Utility functions for test cases
  !-------------------------------------------------------------------------

  use os_util
  use print_util
  
  implicit none

  integer :: nfails = 0 
  integer :: nok = 0 
  integer :: itime_started = 0

  contains
  
    subroutine test_header (message)  
      !! print header for a test section  
      character (*), intent(in)   :: message
      character (:), allocatable  :: line, text
      integer   :: ib, ie

      line = repeat('-',80)
      text = " " // message // " "
      ib = 3
      ie = ib + len(text) - 1
      line (ib:ie) = text
  
      write (*,*) 
      call print_text(line)
      write (*,*) 
  
    end subroutine 
  
    module subroutine test_footer (message)   
      !! print header for a test section with number of fails   

      
      use, intrinsic:: iso_fortran_env, only: stdin=>input_unit

      character (*), intent(in) :: message
  
      write (*,*) 
  
      if (nfails == 0) then 
        call print_colored (COLOR_NOTE, " -- Finished "//message//" ------ ")
        call print_colored (COLOR_GOOD, stri(nok)//" tests passed successfully")
      else
        call print_colored (COLOR_NOTE, " -- Finished "//message//" ------ ")
        call print_colored (COLOR_ERROR, stri(nfails)//" errors")
      end if 
      write (*,*) 
      write (*,*) 


      ! print *, 'Enter to continue ...'
      ! read(stdin,*)
    
    end subroutine 
  
    
    module subroutine assertf (val1, val2, message, decimals)  
      !! compare two float numbers, print message, return 1 if not equal 
  
      double precision, intent(in)  :: val1, val2
      character (*), intent(in)     :: message
      integer, intent (in), optional :: decimals
      integer   :: dec
      character (:), allocatable ::format_string, val1s, val2s

      if (.not. present (decimals)) then 
        dec = 7
      else 
        dec = decimals         
      end if
      
      format_string = "(f10."//stri(dec)//")"

      if (val2 == 0d0) then                             ! handle -0.0 
        val1s = strf (format_string, abs(val1))
      else
        val1s = strf (format_string, val1)
      end if 
      val2s = strf (format_string, val2)

      if (val1s == val2s) then
        call print_action (message//" - Ok")
        nok = nok + 1
      else
        nfails = nfails + 1
        call print_action (message//" - ", no_crlf=.true.)
        call print_colored (COLOR_ERROR, "Failed")
        print *
        call print_text  ("     val1: "//val1s//"  <->  val2: "//val2s)
      end if 
  
    end subroutine 
  
  
    module subroutine asserti (val1, val2, message) 
      !! compare two float numbers, print message, return 1 if not equal 
      !!
      integer, intent(in)           :: val1, val2
      character (*), intent(in)     :: message
  
      if (val1 == val2) then
        call print_action (message//" - Ok")
        nok = nok + 1
      else
        nfails = nfails + 1
        call print_action (message//" - ", no_crlf=.true.)
        call print_colored (COLOR_ERROR, "Failed")
        print *
        call print_text  ("     val1: "//stri(val1)//"  <->  val2: "//stri(val2))

      end if 
  
    end subroutine
  

    subroutine timing_start ()
      !! start the timer for timing measurements
   
      call system_clock(count=itime_started)

    end subroutine

    subroutine timing_result (message)
      !! start the timer for timing measurements
      character (*), intent(in) :: message

      integer :: itime_finish, rate
      double precision :: time_diff

      call system_clock(count_rate=rate)
      call system_clock(count=itime_finish)
      time_diff = real (itime_finish-itime_started)/real(rate)
      call print_colored (COLOR_FEATURE, "   - Time: "// message //"  "//strf('(f5.4)', time_diff)//"s")
      write (*,*)

    end subroutine

end module
    

