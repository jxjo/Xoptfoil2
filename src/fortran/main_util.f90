! MIT License
! Copyright (c) 2024 Jochen Guenzel

!
! helper functions for main 
!

module main_util

  use os_util

  implicit none

  contains


  subroutine clean_old_output ()

    !! remove subdirectory for all the design files, clean existing files 

    use commons,      only: output_prefix, design_subdir, DESIGN_SUBDIR_POSTFIX

    design_subdir = output_prefix // DESIGN_SUBDIR_POSTFIX // '/'
    call remove_directory (design_subdir)
  
    call delete_file (output_prefix//'.dat')              ! the final airfoil 
    call delete_file (output_prefix//'.hicks')            ! ... could have been hicks henne
    call delete_file (output_prefix//'.bez')              ! ... could have been bezier 
  
  end subroutine 

end module
