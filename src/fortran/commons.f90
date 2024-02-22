! MIT License
! Copyright (c) 2022-2024 Jochen Guenzel

module commons

  ! global types and commons 

  implicit none

  ! --- global statics ------------------------------------------------------------ 

  logical                     :: show_details                       ! Show more infos during optimization

  character (*), parameter    :: DESIGN_SUBDIR_POSTFIX = '_temp'
  character (:), allocatable  :: design_subdir                      ! temp directory for all intermediate results
  character (:), allocatable  :: output_prefix                      ! the result output name (of airfoil)


end module commons
