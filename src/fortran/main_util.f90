! MIT License
! Copyright (c) 2024 Jochen Guenzel

!
! helper functions for main 
!

module main_util

  use os_util

  implicit none

  contains

  subroutine write_final_foil (foil)

    !-----------------------------------------------------------------------------
    !! write final .dat and bezier files 
    !-----------------------------------------------------------------------------

    use commons,            only : airfoil_type, output_prefix
    use airfoil_operations, only : airfoil_write
    use shape_bezier,       only : write_bezier_file
    use shape_hicks_henne,  only : write_hh_file

    type (airfoil_type), intent(in) :: foil 

    character (:), allocatable      :: output_file

    output_file = output_prefix//'.dat'
    call airfoil_write (output_file, output_prefix, foil)
  
    if (foil%is_bezier_based) then
      output_file = output_prefix//'.bez'
      call print_colored (COLOR_NOTE, "   Writing bezier  to ")
      call print_colored (COLOR_HIGH, output_file)
      write (*,*)
      call write_bezier_file (output_file, output_prefix, foil%top_bezier, foil%bot_bezier)
  
    else if (foil%is_hh_based) then
      output_file = output_prefix//'.hicks'
      call print_colored (COLOR_NOTE, "   Writing hicks   to ")
      call print_colored (COLOR_HIGH, output_file)
      write (*,*)
      call write_hh_file (output_file, output_prefix, foil%top_hh, foil%bot_hh)
    end if 
  
  end subroutine 

end module
