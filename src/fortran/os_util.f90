! MIT License
! Copyright (c) 2023 jxjo

module os_util

  !------------------------------------------------------------------------------------------
  !  OS dependant utility functions like colored console output
  !------------------------------------------------------------------------------------------

#ifdef UNIX
#else
  use ISO_C_BINDING
#endif    

  implicit none

  ! --- Global Constants -----------------------------------------------------

  integer, parameter, public            :: NOT_DEF_I = -99999999
  double precision, parameter, public   :: NOT_DEF_D = -99999999d0

  ! --- Global Print Parameters ----------------------------------------------

  integer, parameter, public  :: COLOR_GOOD    = 1
  integer, parameter, public  :: COLOR_BAD     = 2
  integer, parameter, public  :: COLOR_NORMAL  = 3
  integer, parameter, public  :: COLOR_HIGH    = 4
  integer, parameter, public  :: COLOR_ERROR   = 5
  integer, parameter, public  :: COLOR_WARNING = 6
  integer, parameter, public  :: COLOR_NOTE    = 7
  integer, parameter, public  :: COLOR_FEATURE = 8
  integer, parameter, public  :: COLOR_PALE    = 9
  integer, parameter, public  :: COLOR_FIXED   = 10

  integer, parameter, public  :: Q_GOOD     = 0         ! must be ordered
  integer, parameter, public  :: Q_OK       = 1
  integer, parameter, public  :: Q_BAD      = 2
  integer, parameter, public  :: Q_PROBLEM  = 4
  integer, parameter, public  :: Q_NEW      = 8
  integer, parameter, public  :: Q_NO       = 16

  private

  public :: to_lower
  public :: stri, strf

  public :: make_directory
  public :: remove_directory
  public :: delete_file
  public :: path_join
  public :: filename_stem
  public :: filename_suffix

  public :: print_colored
  public :: print_colored_i
  public :: print_colored_r
  public :: print_colored_s
  public :: print_colored_rating
  public :: i_quality
  public :: r_quality
  
  public :: my_stop
  public :: set_my_stop_to_stderr 


  interface print_colored
#ifdef UNIX
  module procedure print_colored
#else
  module procedure print_colored_windows
#endif    
  end interface

  interface make_directory
#ifdef UNIX
 module procedure make_directory_unix
#else
  module procedure make_directory_windows
#endif    
  end interface

  interface remove_directory
#ifdef UNIX
  module procedure remove_directory_unix
#else
  module procedure remove_directory_windows
#endif    
  end interface
  
  interface path_join
#ifdef UNIX
  module procedure path_join_unix
#else
  module procedure path_join_windows
#endif    
  end interface

    
  interface delete_file
#ifdef UNIX
  module procedure delete_file_unix
#else
  module procedure delete_file_windows
#endif    
  end interface

!------------------------------------------------------------------------------------------
!  unix  specific 
!------------------------------------------------------------------------------------------
#ifdef UNIX

  character (4)  :: FOREGROUND_YELLOW      = '[33m'
  character (4)  :: FOREGROUND_MAGENTA_UX  = '[35m'
  character (4)  :: FOREGROUND_CYAN        = '[36m'
  character (4)  :: FOREGROUND_GRAY        = '[2m'          ! dimm
  character (4)  :: FOREGROUND_LIGHT_GREEN = '[92m'
  character (4)  :: FOREGROUND_LIGHT_RED   = '[91m'
  character (4)  :: FOREGROUND_LIGHT_BLUE  = '[94m'
  character (4)  :: FOREGROUND_BOLD        = '[1m'
  character (4)  :: FOREGROUND_DEFAULT     = '[0m'          ! '[39m'
  
!------------------------------------------------------------------------------------------
!  windows specific 
!------------------------------------------------------------------------------------------
#else
  integer, parameter, public :: BOOL = C_INT
  integer, parameter, public :: HANDLE = C_INTPTR_T
  integer, parameter, public :: ULONG = C_LONG
  integer, parameter, public :: SHORT = C_SHORT
  integer, parameter, public :: WORD = C_SHORT
  integer, parameter, public :: DWORD = C_LONG
  integer(DWORD), parameter, public :: STD_OUTPUT_HANDLE = -11
  integer(WORD), parameter, public :: FOREGROUND_BLUE = int(Z"1",WORD)
  integer(WORD), parameter, public :: FOREGROUND_GREEN = int(Z"2",WORD)
  integer(WORD), parameter, public :: FOREGROUND_RED = int(Z"4",WORD)
  integer(WORD), parameter, public :: FOREGROUND_MAGENTA= int(Z"5",WORD)
  integer(WORD), parameter, public :: FOREGROUND_INTENSITY = int(Z"8",WORD)
  integer(WORD), parameter, public :: BACKGROUND_BLUE = int(Z"10",WORD)
  integer(WORD), parameter, public :: BACKGROUND_GREEN = int(Z"20",WORD)
  integer(WORD), parameter, public :: BACKGROUND_RED = int(Z"40",WORD)
  integer(WORD), parameter, public :: BACKGROUND_INTENSITY = int(Z"80",WORD)
  integer(WORD), parameter, public :: COMMON_LVB_LEADING_BYTE = int(Z"100",WORD)
  integer(WORD), parameter, public :: COMMON_LVB_TRAILING_BYTE = int(Z"200",WORD)
  integer(WORD), parameter, public :: COMMON_LVB_GRID_HORIZONTAL = int(Z"400",WORD)
  integer(WORD), parameter, public :: COMMON_LVB_GRID_LVERTICAL = int(Z"800",WORD)
  integer(WORD), parameter, public :: COMMON_LVB_GRID_RVERTICAL = int(Z"1000",WORD)
  integer(WORD), parameter, public :: COMMON_LVB_REVERSE_VIDEO = int(Z"4000",WORD)

  type, bind(C), public :: T_COORD
    integer(SHORT) x
    integer(SHORT) y
  end type T_COORD
  type, bind(C), public :: T_SMALL_RECT
    integer(SHORT) Left
    integer(SHORT) Top
    integer(SHORT) Right
    integer(SHORT) Bottom
  end type T_SMALL_RECT
  type, bind(C), public :: T_CONSOLE_SCREEN_BUFFER_INFO
    type(T_COORD) dwSize
    type(T_COORD) dwCursorPosition
    integer(WORD) wAttributes
    type(T_SMALL_RECT) srWindow
    type(T_COORD) dwMaximumWindowSize
  end type T_CONSOLE_SCREEN_BUFFER_INFO

  public GetConsoleScreenBufferInfo
  interface
    function GetConsoleScreenBufferInfo(hConsoleOutput, &
        lpConsoleScreenBufferInfo) bind(C,name='GetConsoleScreenBufferInfo')
        import BOOL, HANDLE, T_CONSOLE_SCREEN_BUFFER_INFO
        implicit none
!gcc$ attributes stdcall :: GetConsoleScreenBufferInfo
        integer(BOOL) GetConsoleScreenBufferInfo
        integer(HANDLE), value :: hConsoleOutput
        type(T_CONSOLE_SCREEN_BUFFER_INFO) lpConsoleScreenBufferInfo
    end function GetConsoleScreenBufferInfo
  end interface

!   public GetStdHandle
  interface
    function GetStdHandle(nStdHandle) bind(C,name='GetStdHandle')
        import HANDLE, DWORD
        implicit none
!gcc$ attributes stdcall :: GetStdHandle
        integer(HANDLE) GetStdHandle
        integer(DWORD), value :: nStdHandle
    end function GetStdHandle
  end interface
!   public SetConsoleMode
  interface
    function SetConsoleMode(hConsoleHandle, dwMode) bind(C,name='SetConsoleMode')
        import HANDLE, DWORD, BOOL
        implicit none
!gcc$ attributes stdcall :: SetConsoleMode
        integer(BOOL) SetConsoleMode
        integer(HANDLE), value:: hConsoleHandle
        integer(DWORD), value :: dwMode
    end function SetConsoleMode
  end interface

  public SetConsoleTextAttribute
  interface
    function SetConsoleTextAttribute(hConsoleOutput, wAttributes) &
        bind(C,name='SetConsoleTextAttribute')
        import HANDLE, WORD, BOOL
        implicit none
!gcc$ attributes stdcall :: SetConsoleTextAttribute
        integer(BOOL) SetConsoleTextAttribute
        integer(HANDLE), value :: hConsoleOutput
        integer(WORD), value :: wAttributes
    end function SetConsoleTextAttribute
  end interface
#endif

  ! ----- static, private 

    logical       :: myStop_out_to_console = .true. 

  contains
  
!------------------------------------------------------------------------------------------
!  Print a string to console into a color defined by wAttributes 
!------------------------------------------------------------------------------------------
#ifdef UNIX

subroutine print_colored (color_typ, text)

  integer, intent (in) :: color_typ
  character(*),  intent (in) :: text
  character (20) :: color_string, normal_string

!$omp critical (print_colored)

  select case (color_typ)
    case (COLOR_GOOD)
      color_string = FOREGROUND_LIGHT_GREEN
    case (COLOR_BAD)
      color_string = FOREGROUND_LIGHT_RED
    case (COLOR_HIGH)
      color_string = FOREGROUND_BOLD
    case (COLOR_ERROR)
      color_string = FOREGROUND_LIGHT_RED
    case (COLOR_WARNING)
      color_string = FOREGROUND_YELLOW
    case (COLOR_NOTE)
      color_string = FOREGROUND_GRAY
    case (COLOR_PALE)
      color_string = FOREGROUND_GRAY
    case (COLOR_FIXED)
      color_string = FOREGROUND_MAGENTA_UX
    case default
      color_string = ''
  end select

  if (len(trim(color_string)) > 0) then 
    color_string  = achar(27) // color_string
    normal_string = achar(27) // FOREGROUND_DEFAULT
  else
    color_string  = ''
    normal_string = '' 
  end if

  write(*,'(A)', advance = 'no') trim(color_string) // text // trim(normal_string)

!$omp end critical (print_colored)

end subroutine print_colored

#else
  
subroutine print_colored_windows (color_typ, text)

  integer, intent (in) :: color_typ
  integer(WORD) :: wAttributes, color_attribute
  character(*),  intent (in) :: text
  
  integer(HANDLE) :: hConsoleOutput
  integer(BOOL) :: iresult
  type(T_CONSOLE_SCREEN_BUFFER_INFO) lpConsoleScreenBufferInfo

!$omp critical (print_colored_windows)

  select case (color_typ)
    case (COLOR_GOOD)
      color_attribute = iany([FOREGROUND_GREEN, FOREGROUND_INTENSITY])
    case (COLOR_BAD)
      color_attribute = iany([FOREGROUND_RED])
    case (COLOR_HIGH)
      color_attribute = iany([FOREGROUND_RED, FOREGROUND_GREEN, FOREGROUND_BLUE, FOREGROUND_INTENSITY])
    case (COLOR_ERROR)
      color_attribute = iany([FOREGROUND_RED, FOREGROUND_INTENSITY])
    case (COLOR_WARNING)
      color_attribute = iany([FOREGROUND_RED, FOREGROUND_GREEN])
    case (COLOR_NOTE)
      color_attribute = iany([FOREGROUND_INTENSITY])
      !color_attribute = iany([FOREGROUND_BLUE, FOREGROUND_GREEN])
    case (COLOR_PALE)
      color_attribute = iany([FOREGROUND_INTENSITY])
    case (COLOR_FIXED)
      color_attribute = iany([FOREGROUND_MAGENTA])
    case (COLOR_FEATURE)
      color_attribute = iany([FOREGROUND_BLUE, FOREGROUND_INTENSITY])
    case default
      color_attribute = iany([FOREGROUND_RED, FOREGROUND_GREEN, FOREGROUND_BLUE])
  end select

  wAttributes = int(color_attribute, 2) 
  
  hConsoleOutput = GetStdHandle(STD_OUTPUT_HANDLE)
  iresult = GetConsoleScreenBufferInfo(hConsoleOutput,lpConsoleScreenBufferInfo)
  
  iresult = SetConsoleTextAttribute(hConsoleOutput,wAttributes)
  write(*,'(A)', advance = 'no') text
  
  wAttributes = int(iany([FOREGROUND_RED, FOREGROUND_GREEN, FOREGROUND_BLUE]),2)
  ! Switch back to normal color instead of restoring old value 
  !        (problems with multi threaded screen write)
  ! iresult = SetConsoleTextAttribute(hConsoleOutput,lpConsoleScreenBufferInfo%wAttributes)
  iresult = SetConsoleTextAttribute(hConsoleOutput,wAttributes)

!$omp end critical (print_colored_windows)

end subroutine print_colored_windows

#endif

!------------------------------------------------------------------------------------------
!  File functions 
!------------------------------------------------------------------------------------------

#ifdef UNIX

  subroutine make_directory_unix (subdirectory, preserve_existing)
    character(*),  intent (in) :: subdirectory
    logical,  intent (in), optional :: preserve_existing
    integer         :: istat
    character (255) :: command
    logical         :: remove_existing 

    remove_existing = .true. 

    if (present(preserve_existing)) then
      remove_existing = .not. preserve_existing
    end if 

    ! Single quotes ignore any special characters. 
    ! Double quotes ignores all except $, back quotes and baclslashes.

    if (remove_existing) then
      command = "rmdir --ignore-fail-on-non-empty '"//trim(subdirectory)//"'"
      istat = system (trim(command))
    end if 

    command = "mkdir -p '"//trim(subdirectory)//"'"
    istat = system (trim(command))

  end subroutine 



  subroutine remove_directory_unix (subdirectory)
    character(*),  intent (in) :: subdirectory
    integer         :: istat
    character (255) :: command

    command = "rm -rf '"//trim(subdirectory)//"'"
    istat = system (trim(command))
  end subroutine 



  subroutine delete_file_unix (file_path)

    !! delete file(s) having file_path (which can have wildcards) 

    character(*),  intent (in) :: file_path
    integer         :: istat
    character (255) :: command

    command = "rm -f '"//trim(file_path)//"'"
    istat = system (trim(command))

  end subroutine 

  

  function path_join_unix (dir, file_name) result (path) 

    !! returns dir and file_name concatenated with '/'
  
    character (*), intent(in)       :: dir , file_name
    character (:), allocatable      :: path 
  
    if (dir /= "") then 
      if (dir (len(dir):len(dir)) /= "/") then 
        path = dir // "/"//file_name
      else
        path = dir //file_name
      end if 
    else 
      path = file_name
    end if 
  
  end function 

#else


  ! --- windows -------------------------------------------------------------


  subroutine make_directory_windows (subdirectory, preserve_existing)

    !! make directory - 'preserve' an existing directory is not removed 

    character(*),  intent (in) :: subdirectory
    logical,  intent (in), optional :: preserve_existing
    integer         :: istat
    character (255) :: command
    logical         :: remove_existing 

    remove_existing = .true. 

    if (present(preserve_existing)) then
      remove_existing = .not. preserve_existing
    end if 

    if (remove_existing) then
      command = 'if exist "'//trim(subdirectory)//'" rmdir "'//trim(subdirectory)//'" /s/q'
      istat = system (trim(command))
    end if 

    command = 'if not exist "'//trim(subdirectory)//'" mkdir "'//trim(subdirectory)//'"'
    istat = system (trim(command))

  end subroutine 



  subroutine remove_directory_windows (subdirectory)
    character(*),  intent (in) :: subdirectory
    integer         :: istat
    character (255) :: command

    command = 'if exist "'//trim(subdirectory)//'" rmdir "'//trim(subdirectory)//'" /s/q'
    istat = system (trim(command))
  end subroutine 



  subroutine delete_file_windows (file_path)

    !! delete file(s) having file_path (which can have wildcards) 

    character(*),  intent (in) :: file_path
    integer         :: istat
    character (255) :: command

    ! use del - 'file not found' message has to be suppressed
    command = 'del "'//trim(file_path)//'" /q > nul 2> nul'
    istat = system (trim(command))

  end subroutine 



  function path_join_windows (dir, file_name) result (path) 

    !! returns dir and file_name concatenated with '\'

    character (*), intent(in)       :: dir , file_name
    character (:), allocatable      :: path 

    if (dir /= "") then 
      if (dir (len(dir):len(dir)) /= "\") then 
        path = dir // "\"//file_name
      else
        path = dir //file_name
      end if 
    else 
      path = file_name
    end if 

  end function 

#endif


  function filename_stem (file_name) result (stem) 

    !! returns filename without file extensions

    character (*), intent(in)       :: file_name
    character (:), allocatable      :: stem, name 
    logical                         :: dot_found 
    integer                         :: i 

    stem = '' 
    name = trim(file_name)
    if (len(name) == 0) return 

    ! find first '.' going backward from the end 

    dot_found = .false. 
    
    do i = len(name), 1, -1
      if (name(i:i) == ".") then 
        dot_found = .true.
        exit 
      elseif (name(i:i) == "/" .or. name(i:i) == "\") then 
        return 
      end if 
    end do 

    ! substring if dot found 

    if (dot_found) then 
      stem = name (1:i-1)
    else
      stem = name 
    end if 

  end function 



  function filename_suffix (file_name) result (suffix) 

    !! returns file extensions (e.g. '.dat') of filename 

    character (*), intent(in)       :: file_name
    character (:), allocatable      :: suffix, name 
    logical                         :: dot_found 
    integer                         :: i 

    suffix = '' 
    name = trim(file_name)
    if (len(name) == 0) return 

    ! find first '.' going backward from the end 

    dot_found = .false. 
    
    do i = len(name), 1, -1
      if (name(i:i) == ".") then 
        dot_found = .true.
        exit 
      elseif (name(i:i) == "/" .or. name(i:i) == "\") then 
        return 
      end if 
    end do 

    ! substring if dot found 

    if (dot_found) then 
      suffix = name (i:len(name))
    end if 

  end function 


!------------------------------------------------------------------------------------------
!  String functions - Integer  and Float to string 
!------------------------------------------------------------------------------------------

  pure function to_lower (strIn) result(strOut)

    !! string to lowercase 
    ! Adapted from http://www.star.le.ac.uk/~cgp/fortran.html (25 May 2012)
    ! Original author: Clive Page
    
      implicit none

      character(len=*), intent(in) :: strIn
      character(len=len(strIn)) :: strOut
      integer :: i,j

      do i = 1, len(strIn)
          j = iachar(strIn(i:i))
          if (j>= iachar("A") .and. j<=iachar("Z") ) then
                strOut(i:i) = achar(iachar(strIn(i:i))+32)
          else
                strOut(i:i) = strIn(i:i)
          end if
      end do
    
    end function to_lower


  pure function stri (a_int, length)

    !! integer to string 
    !! length: optional - fixed length, right adjusted

    integer,  intent (in) :: a_int
    integer,  intent (in), optional :: length

    character (:), allocatable :: stri
    character (10) :: as_string
    integer        :: l 

    write (as_string, '(I10)') a_int

    if (present (length)) then
      l = min (len(as_string), length)
      stri = as_string (10-l+1:)
    else
      stri = trim(adjustl(as_string))
    end if 

  end function 



  pure function strf (format, a_float, fix) result (as_string)

    !! real to string using format specifier 
    !! format: specifier string like '(f7.2)'
    !! fix: optional - fixed length, right adjusted 
  
    doubleprecision,  intent (in) :: a_float
    character (*),  intent (in) :: format
    logical,  intent (in), optional :: fix

    character (:), allocatable :: as_string
    logical :: do_adjustl

    if (trim(format) == '') return

    as_string = repeat(' ',20)
    write (as_string, format) a_float

    if (present (fix)) then
      do_adjustl = .not. fix
    else 
      do_adjustl = .true. 
    end if  

    if (do_adjustl) then 
      as_string = trim(adjustl(as_string))
    else 
      as_string = trim(as_string)
    end if 


  end function 


subroutine my_stop(message) 

  !------------------------------------------------------------------------------------------
  !! Prints an error message to stderr, or just warns and stops program exec
  !------------------------------------------------------------------------------------------

  use, intrinsic:: iso_fortran_env, only: stderr=>error_unit

  character(*), intent(in) :: message

  print *

  if (myStop_out_to_console) then 
    call print_colored (COLOR_ERROR, " Error: ")
    call print_colored (COLOR_NORMAL, trim(message))
    print *
    print *
  else 
    ! write error message to stderr - which is (hopefully) 0 
    write (stderr,*) 'Error: '// message
  end if 

  ! stop and end 
  stop 1 

end subroutine my_stop

subroutine set_my_stop_to_stderr (aBool)

  logical, intent(in)   :: aBool 

  !! if .true. write the my_stop errormessage to stderr unit (not stdout) 
  
  if (aBool) then 
    myStop_out_to_console = .false. 
  end if 

end subroutine 

  
subroutine print_colored_i (strlen, quality, ivalue)
  
  !-------------------------------------------------------------------------
  ! prints the integer ivalue colored depending on its quality (e.g. Q_OK)
  !   if strlen == 0  then ivalue will be left adjusted  
  !-------------------------------------------------------------------------

  integer, intent(in) :: ivalue, quality, strlen

  character (strlen)  :: str
  character (10)      :: tmp_str
  integer             :: color 

  if (strlen < 0 .or. strlen > 10) return 

  select case (quality)
    case (Q_GOOD)
      color = COLOR_GOOD
    case (Q_OK)
      color = COLOR_NORMAL
    case (Q_BAD)
      color = COLOR_WARNING
    case (Q_NEW)
      color = COLOR_FEATURE
    case (Q_PROBLEM)
      color = COLOR_BAD
    case default 
      color = COLOR_NOTE
  end select

  if (strlen == 0) then 
    call print_colored (color, stri(ivalue))
  else 

    str = repeat('*',strlen)

    if ((ivalue >= 10**strlen) .or. (ivalue <= -10**(strlen-1))) then
      str = repeat('*',strlen)
    else
      write (tmp_str, '(I10)') ivalue
      str = tmp_str ((len(tmp_str)-strlen+1):len(tmp_str))
    endif

    call print_colored (color, str)

  endif

  
end subroutine print_colored_i

!-------------------------------------------------------------------------
! prints the real rvalue colored depending
!   on its quality (e.g. Q_OK)
!-------------------------------------------------------------------------
  
subroutine print_colored_r (strlen, format_string, quality, rvalue)
  
  double precision, intent(in) :: rvalue
  integer, intent(in)          :: strlen, quality
  character (*), intent(in)    :: format_string

  character (strlen)  :: str
  integer             :: color, idec

  select case (quality)
    case (Q_GOOD)
      color = COLOR_GOOD
    case (Q_OK)
      color = COLOR_NORMAL
    case (Q_BAD)
      color = COLOR_WARNING
    case (Q_NEW)
      color = COLOR_FEATURE
    case (Q_PROBLEM)
      color = COLOR_BAD
    case default 
      color = COLOR_NOTE
  end select

  write (str,format_string) rvalue  
  str = adjustr(str)

  ! remove decimal point at the end if there are no decimals 
  idec = index (str,".")
  if (idec == len (str) .and. idec > 1) then 
    str = " " // str (1: idec-1)           ! shift right
  end if 

  call print_colored (color, str)

end subroutine print_colored_r

!-------------------------------------------------------------------------
! evalutes the quality (constant Q_GOOD etc) of a integer value 
!-------------------------------------------------------------------------

function i_quality (value, lim_good, lim_ok, lim_bad)

  integer, intent(in) :: value, lim_good, lim_ok, lim_bad
  integer :: i_quality

  if (value < lim_good) then
    i_quality = Q_GOOD
  elseif (value < lim_ok) then
    i_quality = Q_OK
  elseif (value < lim_bad) then
    i_quality = Q_BAD
  else
    i_quality = Q_PROBLEM
  end if

end function i_quality
  
!-------------------------------------------------------------------------
! evalutes the quality (constant Q_GOOD etc) of a real value 
!-------------------------------------------------------------------------

function r_quality (value, lim_good, lim_ok, lim_bad)

  double precision, intent(in) :: value, lim_good, lim_ok, lim_bad
  integer :: r_quality

  if (value < lim_good) then
    r_quality = Q_GOOD
  elseif (value < lim_ok) then
    r_quality = Q_OK
  elseif (value < lim_bad) then
    r_quality = Q_BAD
  else
    r_quality = Q_PROBLEM
  end if

end function r_quality

!-------------------------------------------------------------------------
! prints the string colored depending  on its quality (e.g. Q_OK)
!-------------------------------------------------------------------------
  
subroutine print_colored_s (quality, str)
  
  integer, intent(in)       :: quality
  character (*), intent(in) :: str

  integer  :: color 

  select case (quality)
    case (Q_GOOD)
      color = COLOR_GOOD
    case (Q_OK)
      color = COLOR_NORMAL
    case (Q_BAD)
      color = COLOR_WARNING
    case (Q_NEW)
      color = COLOR_FEATURE
    case (Q_PROBLEM)
      color = COLOR_BAD
    case default 
      color = COLOR_NOTE
  end select

  call print_colored (color, str)
  
end subroutine print_colored_s

!-------------------------------------------------------------------------
! prints a colored rating based on quality (e.g. Q_OK) with a lenght of strlen
!-------------------------------------------------------------------------
  
subroutine print_colored_rating (strlen, quality)
  
  integer, intent(in)      :: quality, strlen

  character (strlen)  :: str, comment
  integer             :: color 

  select case (quality)
    case (Q_GOOD)
      color = COLOR_GOOD
      comment ='perfect'
    case (Q_OK)
      color = COLOR_NORMAL
      comment ='ok'
    case (Q_BAD)
      color = COLOR_WARNING
      comment ='bad'
    case (Q_NEW)
      color = COLOR_FEATURE
      comment ='new'
    case default 
      color = COLOR_BAD
      comment ='critical'
  end select
  write (str,'(A)') comment
  str = adjustl(str)
  call print_colored (color, str)
  
end subroutine print_colored_rating


!-------------------------------------------------------------------------
! measure time to run 
!-------------------------------------------------------------------------
  
! integer         :: itime_start, itime_finish, rate
! doubleprecision :: time_diff

! call system_clock(count_rate=rate)
! call system_clock(itime_start)
! ............
! call system_clock(itime_finish)
! time_diff = real (itime_finish-itime_start)/real(rate)
! print '("Time = ",f6.3," seconds"',time_diff


end module os_util