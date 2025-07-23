! MIT License
! Copyright (c) 2025 Jochen Guenzel 

!
! print utility functions 
!

module print_util 

  use os_util,  only: COLOR_NORMAL, COLOR_NOTE, COLOR_ERROR, COLOR_WARNING, COLOR_PALE, COLOR_FEATURE
  use os_util,  only: print_colored 

  implicit none
  private

  public :: set_show_details
  public :: print_header, print_action
  public :: print_error, print_warning, print_note, print_text, print_fixed
  public :: quoted

  ! ---- static, private ---------------------------------

  
  logical :: show_details = .false.                 ! common switch for showing details  

contains

  subroutine set_show_details (show)

    !! set the local show_detail which controls print output 

    logical, intent(in)   :: show 

    show_details = show

    ! if (show) print *

  end subroutine 



  subroutine print_header (text, highlighted_text, no_crlf)

    ! print a header text line with an optional highlighted_text

    character (*), intent(in)           :: text 
    character (*), intent(in),optional  :: highlighted_text 
    logical, intent (in), optional      :: no_crlf
    logical                             :: do_crlf

    if (present (no_crlf)) then 
      do_crlf = .not. no_crlf
    else
      do_crlf = .true. 
    end if 
    
    if (show_details) then 
      print * 
      call print_colored (COLOR_NORMAL, " - "//text)
      if (present (highlighted_text)) then 
        call print_colored (COLOR_NORMAL, " "//highlighted_text)
      end if  
    else
      call print_colored (COLOR_NOTE, " - "//text)
      if (present (highlighted_text)) then 
        call print_colored (COLOR_NORMAL, " "//highlighted_text)
      end if  
    end if  

    if (do_crlf) then 
      print * 
      if (show_details)  print * 
    end if 

  end subroutine 


  subroutine print_action (text, highlighted_text, no_crlf)

    ! print an action text line (only when 'show_details')

    character (*), intent(in)           :: text 
    character (*), intent(in),optional  :: highlighted_text 
    logical, intent (in), optional      :: no_crlf
    integer                     :: i 

    if (.not. show_details) return 

    i = 3 
    call print_colored (COLOR_NOTE, repeat(' ',i))
    call print_colored (COLOR_NORMAL, "- ")
    call print_colored (COLOR_NOTE, text)
    if (present (highlighted_text)) then 
      call print_colored (COLOR_NORMAL, " "//highlighted_text)
    end if 

    if (present (no_crlf)) then 
      if (.not. no_crlf) then 
        print * 
      end if 
    else
      print * 
    end if 

  end subroutine 

  

  subroutine print_error (text, indent)

    !! print colored error message 

    character(*),  intent (in)      :: text
    integer, intent (in), optional  :: indent
    integer :: i
    i = 1
    if (present (indent)) then 
      if (indent >0 .and. indent <80) i = indent
    end if
    call print_colored (COLOR_NORMAL, repeat(' ',i))
    call print_colored (COLOR_ERROR, trim(text))
    print *    
  end subroutine 
  


  subroutine print_warning (text, indent)

    !! print colored warning message 

    character(*), intent (in)       :: text
    integer, intent (in), optional  :: indent
    integer :: i
    i = 1
    if (present (indent)) then 
      if (indent >0 .and. indent <80) i = indent
    end if
    call print_colored (COLOR_NORMAL, repeat(' ',i))
    call print_colored (COLOR_WARNING, 'Warning: ')
    call print_colored (COLOR_NORMAL, text)
    print *
     
  end subroutine print_warning
  


  subroutine print_note (text, indent, no_crlf)

    !! print a note with an initial note marker 

    character(*), intent (in)       :: text
    integer, intent (in), optional  :: indent
    logical, intent (in), optional  :: no_crlf
    integer :: i

    if (.not. show_details) return 

    i = 5
    if (present (indent)) then 
      if (indent >= 0 .and. indent < 80) i = indent
    end if
    call print_colored (COLOR_FEATURE, repeat(' ',i) // '> ')
    call print_colored (COLOR_PALE, text)

    if (present (no_crlf)) then 
      if (.not. no_crlf) print * 
    else
      print * 
    end if 

  end subroutine print_note
  

  subroutine print_text (text, indent, no_crlf)

    !! print a note text with an optional intent

    character(*), intent (in)       :: text
    integer, intent (in), optional  :: indent
    logical, intent (in), optional  :: no_crlf
    integer :: i
    i = 1
    if (present (indent)) then 
      if (indent >0 .and. indent <80) i = indent
    end if
    call print_colored (COLOR_NOTE, repeat(' ',i) // text)

    if (present (no_crlf)) then 
      if (.not. no_crlf) print * 
    else
      print * 
    end if 
     
  end subroutine print_text
  


  subroutine print_fixed (text, length, adjust_right)

    !! print text as note in fixed length, either left or right adjusted

    character(*), intent (in)       :: text
    integer, intent (in)            :: length
    logical, intent (in), optional  :: adjust_right
    character (:), allocatable      :: t, filler
    logical                     :: right

    if (present (adjust_right)) then 
      right = adjust_right
    else 
      right = .false. 
    end if

    if (len(trim(text)) >= length) then
      t = text (1:length)
    else 
      filler = repeat (" ", length - len(trim(text)))
      if (right) then 
        t = filler // trim(text)
      else 
        t = trim(text) // filler
      end if 
    end if 

    call print_colored (COLOR_NOTE, t)
     
  end subroutine 
  


  function quoted (text)

    !! returns text in quotation marks 

    character(*), intent (in)       :: text
    character (:), allocatable      :: quoted

    quoted = "'" // trim(text) // "'" 
  end function 
    
end module 
