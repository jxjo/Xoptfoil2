! MIT License
! Copyright (c) 2024 jxjo

module string_util

  !-----------------------------------------------------------------------------
  ! String manipulation and JSON parsing utilities
  !-----------------------------------------------------------------------------

  implicit none

  private

  ! String conversions
  public :: to_lower
  public :: stri
  public :: strf
  public :: strf_auto
  public :: strf_dec
  public :: str_duration

  ! File I/O
  public :: read_file_to_string

  ! JSON parsing
  public :: json_get_string
  public :: json_get_section
  public :: json_get_array
  public :: json_get_integer

  ! JSON writing
  public :: json_write_array
  public :: json_write_string
  public :: json_write_integer

contains

  !-----------------------------------------------------------------------------
  ! String manipulation
  !-----------------------------------------------------------------------------

  pure function to_lower (strIn) result(strOut)
    !! String to lowercase 
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
    !! Integer to string 
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
    !! Real to string using format specifier 
    !! format: specifier string like 'f7.2' or '(f7.2)'
    !! fix: optional - fixed length, right adjusted 
  
    doubleprecision,  intent (in) :: a_float
    character (*),  intent (in) :: format
    logical,  intent (in), optional :: fix

    character (:), allocatable :: as_string, fmt
    logical :: do_adjustl

    if (trim(format) == '') return

    ! Add parentheses if not present
    if (format(1:1) == '(') then
      fmt = format
    else
      fmt = '(' // trim(format) // ')'
    end if

    as_string = repeat(' ',20)
    write (as_string, fmt) a_float

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


  pure function strf_auto (width, a_float, no_sign) result (as_string)
  
    !! Real to fixed-width string with maximum decimals.
    !! width: total output width including decimal point.
    !! no_sign: optional - suppress '+' for values known to be non-negative.

    integer, intent(in)          :: width
    doubleprecision, intent(in)  :: a_float
    logical, intent(in), optional :: no_sign

    character (:), allocatable :: as_string
    double precision           :: abs_value
    integer                    :: decimals, integer_digits, min_width, sign_chars

    min_width = max(width, 1)

    abs_value = abs(a_float)
    integer_digits = 1
    if (abs_value >= 1d0) integer_digits = int(log10(abs_value)) + 1

    if (present(no_sign)) then
      if (no_sign) then
        sign_chars = 0
      else
        sign_chars = 1
      end if
    else
      sign_chars = 1
    end if

    if (sign_chars + integer_digits < min_width) then
      decimals = min_width - (sign_chars + integer_digits) - 1
    else
      decimals = 0
    end if

    as_string = strf_dec(min_width, decimals, a_float, no_sign)

  end function



  pure function strf_dec (width, decimals, a_float, no_sign) result (as_string)

    !! double to fixed-width string with number of decimals.
    !! width: total output width including decimal point.
    !! decimals: number of decimals to show.

    integer, intent(in)           :: width, decimals
    doubleprecision, intent(in)   :: a_float
    logical, intent(in), optional :: no_sign

    character (:), allocatable :: as_string, fmt
    integer                    :: min_width, ndecimals
    logical                    :: with_sign

    min_width = max(width, 1)
    ndecimals = max(decimals, 0)
    as_string = repeat(' ', min_width)

    with_sign = .true.
    if (present(no_sign)) with_sign = .not. no_sign

    fmt = repeat(' ', 24)
    if (ndecimals == 0) then
      if (with_sign) then
        write(fmt, '("(SP,I",I0,")")') min_width
      else
        write(fmt, '("(I",I0,")")') min_width
      end if
    else
      if (with_sign) then
        write(fmt, '("(SP,F",I0,".",I0,")")') min_width, ndecimals
      else
        write(fmt, '("(F",I0,".",I0,")")') min_width, ndecimals
      end if
    end if
    if (ndecimals == 0) then
      write(as_string, fmt) nint(a_float)
    else
      write(as_string, fmt) a_float
    end if

  end function



  pure function str_duration (elapsed_seconds) result (as_string)

    !! Elapsed seconds formatted as "1h 02m 03s", "2m 05s", or "17s"

    double precision, intent(in) :: elapsed_seconds

    character(:), allocatable :: as_string
    character(20)             :: buffer
    integer                   :: hours, minutes, seconds
    integer                   :: total_seconds

    total_seconds = max(0, nint(elapsed_seconds))

    hours = total_seconds / 3600
    minutes = mod(total_seconds, 3600) / 60
    seconds = mod(total_seconds, 60)

    if (hours > 0) then
      write(buffer, '(I0,"h ",I2.2,"m ",I2.2,"s")') hours, minutes, seconds
    else if (minutes > 0) then
      write(buffer, '(I0,"m ",I2.2,"s")') minutes, seconds
    else
      write(buffer, '(I0,"s")') seconds
    end if
    as_string = trim(buffer)  

  end function



  !-----------------------------------------------------------------------------
  ! File I/O
  !-----------------------------------------------------------------------------

  function read_file_to_string(filename) result(content)
    !! Read entire file into a single string
    character(*), intent(in) :: filename
    character(:), allocatable :: content
    
    integer :: iunit, ioerr, i
    character(10000) :: buffer
    character(:), allocatable :: temp
    
    iunit = 12
    open(unit=iunit, file=filename, status='old', position='rewind', iostat=ioerr)
    if (ioerr /= 0) then
      print *, 'Error: Cannot find file '//trim(filename)
      stop 1
    end if
    
    content = ''
    do i = 1, 1000
      read(iunit, '(A)', iostat=ioerr) buffer
      if (ioerr /= 0) exit
      temp = content // trim(buffer) // ' '
      content = temp
    end do
    
    close(iunit)
    
  end function


  !-----------------------------------------------------------------------------
  ! JSON parsing
  !-----------------------------------------------------------------------------

  function json_get_string(json_str, key) result(value)
    !! Extract string value from JSON: "key": "value"
    character(*), intent(in) :: json_str
    character(*), intent(in) :: key
    character(:), allocatable :: value
    
    integer :: pos1, pos2, key_pos, colon_pos
    
    value = ''
    
    ! Find "key"
    key_pos = index(json_str, '"'//trim(key)//'"')
    if (key_pos == 0) return
    
    ! Find colon after key
    colon_pos = index(json_str(key_pos:), ':')
    if (colon_pos == 0) return
    colon_pos = colon_pos + key_pos - 1
    
    ! Find opening quote
    pos1 = index(json_str(colon_pos:), '"')
    if (pos1 == 0) return
    pos1 = pos1 + colon_pos - 1
    
    ! Find closing quote
    pos2 = index(json_str(pos1+1:), '"')
    if (pos2 == 0) return
    pos2 = pos2 + pos1
    
    ! Extract value
    value = json_str(pos1+1:pos2-1)
    
  end function


  function json_get_section(json_str, key) result(section)
    !! Extract section content from JSON: "key": { content }
    character(*), intent(in) :: json_str
    character(*), intent(in) :: key
    character(:), allocatable :: section
    
    integer :: key_pos, brace_start, brace_end, i, depth
    character :: c
    
    section = ''
    
    ! Find "key"
    key_pos = index(json_str, '"'//trim(key)//'"')
    if (key_pos == 0) return
    
    ! Find opening brace
    brace_start = index(json_str(key_pos:), '{')
    if (brace_start == 0) return
    brace_start = brace_start + key_pos - 1
    
    ! Find matching closing brace (handle nested braces)
    depth = 1
    brace_end = 0
    do i = brace_start + 1, len(json_str)
      c = json_str(i:i)
      if (c == '{') then
        depth = depth + 1
      else if (c == '}') then
        depth = depth - 1
        if (depth == 0) then
          brace_end = i
          exit
        end if
      end if
    end do
    
    if (brace_end > brace_start) then
      section = json_str(brace_start+1:brace_end-1)
    end if
    
  end function


  function json_get_array(json_str, key) result(values)
    !! Extract array from JSON: "key": [v1, v2, v3]
    character(*), intent(in) :: json_str
    character(*), intent(in) :: key
    double precision, allocatable :: values(:)
    
    integer :: key_pos, bracket_start, bracket_end
    character(:), allocatable :: array_str
    double precision :: temp_values(1000)
    integer :: n, comma_pos, ioerr
    character(:), allocatable :: remaining
    double precision :: val
    
    allocate(values(0))  ! Default empty array
    
    ! Find "key"
    key_pos = index(json_str, '"'//trim(key)//'"')
    if (key_pos == 0) return
    
    ! Find opening bracket
    bracket_start = index(json_str(key_pos:), '[')
    if (bracket_start == 0) return
    bracket_start = bracket_start + key_pos - 1
    
    ! Find closing bracket
    bracket_end = index(json_str(bracket_start:), ']')
    if (bracket_end == 0) return
    bracket_end = bracket_end + bracket_start - 1
    
    ! Extract array content
    array_str = json_str(bracket_start+1:bracket_end-1)
    
    ! Parse comma-separated values
    remaining = trim(adjustl(array_str))
    n = 0
    
    do while (len_trim(remaining) > 0 .and. n < size(temp_values))
      comma_pos = index(remaining, ',')
      
      if (comma_pos > 0) then
        read(remaining(1:comma_pos-1), *, iostat=ioerr) val
        if (ioerr == 0) then
          n = n + 1
          temp_values(n) = val
        end if
        remaining = adjustl(remaining(comma_pos+1:))
      else
        read(remaining, *, iostat=ioerr) val
        if (ioerr == 0) then
          n = n + 1
          temp_values(n) = val
        end if
        exit
      end if
    end do
    
    ! Return properly sized array
    if (n > 0) then
      values = temp_values(1:n)
    end if
    
  end function


  function json_get_integer(json_str, key) result(value)
    !! Extract integer value from JSON: "key": 123
    character(*), intent(in) :: json_str
    character(*), intent(in) :: key
    integer :: value
    
    integer :: key_pos, colon_pos, ioerr
    
    value = 0
    
    ! Find "key"
    key_pos = index(json_str, '"'//trim(key)//'"')
    if (key_pos == 0) return
    
    ! Find colon after key
    colon_pos = index(json_str(key_pos:), ':')
    if (colon_pos == 0) return
    colon_pos = colon_pos + key_pos
    
    ! Read the integer value
    read(json_str(colon_pos:), *, iostat=ioerr) value
    
  end function


  !-----------------------------------------------------------------------------
  ! JSON writing
  !-----------------------------------------------------------------------------

  subroutine json_write_array(iunit, key, values, indent)
    !! Write numeric array in multi-line JSON format
    !! "key": [
    !!     value1,
    !!     value2
    !! ],
    integer, intent(in) :: iunit
    character(*), intent(in) :: key
    double precision, intent(in) :: values(:)
    integer, intent(in) :: indent
    
    integer :: i, n
    
    n = size(values)
    
    write(iunit, '(A,A,A)') repeat(' ', indent), '"' // trim(key) // '": ['
    do i = 1, n
      if (i < n) then
        write(iunit, '(A,F13.10,A)') repeat(' ', indent + 4), values(i), ','
      else
        write(iunit, '(A,F13.10)') repeat(' ', indent + 4), values(i)
      end if
    end do
    write(iunit, '(A,A)') repeat(' ', indent), '],'
    
  end subroutine


  subroutine json_write_string(iunit, key, value, indent, last)
    !! Write string key-value pair in JSON format
    !! "key": "value",  (or without comma if last=.true.)
    integer, intent(in) :: iunit
    character(*), intent(in) :: key, value
    integer, intent(in) :: indent
    logical, intent(in), optional :: last
    
    logical :: is_last
    
    is_last = .false.
    if (present(last)) is_last = last
    
    if (is_last) then
      write(iunit, '(A,A)') repeat(' ', indent), '"' // trim(key) // '": "' // trim(value) // '"'
    else
      write(iunit, '(A,A)') repeat(' ', indent), '"' // trim(key) // '": "' // trim(value) // '",'
    end if
    
  end subroutine


  subroutine json_write_integer(iunit, key, value, indent, last)
    !! Write integer key-value pair in JSON format
    !! "key": 123,  (or without comma if last=.true.)
    integer, intent(in) :: iunit
    character(*), intent(in) :: key
    integer, intent(in) :: value, indent
    logical, intent(in), optional :: last
    
    logical :: is_last
    
    is_last = .false.
    if (present(last)) is_last = last
    
    if (is_last) then
      write(iunit, '(A,A,I0)') repeat(' ', indent), '"' // trim(key) // '": ', value
    else
      write(iunit, '(A,A,I0,A)') repeat(' ', indent), '"' // trim(key) // '": ', value, ','
    end if
    
  end subroutine

end module string_util
