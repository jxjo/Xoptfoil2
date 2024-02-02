! MIT License
! Copyright (c) 2024 Jochen Guenzel 

!
! print utility functions 
!

module print_util 

  use os_util,  only: COLOR_NORMAL, COLOR_NOTE, COLOR_ERROR, COLOR_WARNING, COLOR_PALE
  use os_util,  only: print_colored 

  implicit none
  private

  public :: print_header, print_action
  public :: print_error, print_warning, print_note, print_text 

contains

  subroutine print_header (text)

    ! print a header text line 

    character (*), intent(in)   :: text 
    integer                     :: i 
    i = 1 
    print *
    call print_colored (COLOR_NORMAL, repeat(' ',i))
    call print_colored (COLOR_NORMAL, "- "//text)
    print * 
    print * 

  end subroutine 


  subroutine print_action (text, show, highlighted_text, no_crlf)

    ! print an action text line (only when 'show')

    character (*), intent(in)           :: text 
    logical, intent(in)                 :: show
    character (*), intent(in),optional  :: highlighted_text 
    logical, intent (in), optional      :: no_crlf
    integer                     :: i 

    if (.not. show) return 

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

  

  subroutine print_error (text, intent)

    !! print colored error message 

    character(*),  intent (in)      :: text
    integer, intent (in), optional  :: intent
    integer :: i
    i = 1
    if (present (intent)) then 
      if (intent >0 .and. intent <80) i = intent
    end if
    call print_colored (COLOR_NORMAL, repeat(' ',i))
    call print_colored (COLOR_ERROR, trim(text))
    print *    
  end subroutine 
  


  subroutine print_warning (text, intent)

    !! print colored warning message 

    character(*), intent (in)       :: text
    integer, intent (in), optional  :: intent
    integer :: i
    i = 1
    if (present (intent)) then 
      if (intent >0 .and. intent <80) i = intent
    end if
    call print_colored (COLOR_NORMAL, repeat(' ',i))
    call print_colored (COLOR_WARNING, 'Warning: ')
    call print_colored (COLOR_NORMAL, text)
    print *
     
  end subroutine print_warning
  


  subroutine print_note (text, intent)

    !! print a note with an initial 'Note:'

    character(*), intent (in)       :: text
    integer, intent (in), optional  :: intent
    integer :: i
    i = 5
    if (present (intent)) then 
      if (intent >= 0 .and. intent < 80) i = intent
    end if
    call print_colored (COLOR_WARNING, repeat(' ',i) // '> ')
    call print_colored (COLOR_PALE, trim(text))
    print  *

  end subroutine print_note
  

  subroutine print_text (text, intent, no_crlf)

    !! print a note text with an optional intent

    character(*), intent (in)       :: text
    integer, intent (in), optional  :: intent
    logical, intent (in), optional  :: no_crlf
    integer :: i
    i = 1
    if (present (intent)) then 
      if (intent >0 .and. intent <80) i = intent
    end if
    call print_colored (COLOR_NOTE, repeat(' ',i) // text)

    if (present (no_crlf)) then 
      if (.not. no_crlf) then 
        print * 
      end if 
    else
      print * 
    end if 
     
  end subroutine print_text
  
    
end module 
