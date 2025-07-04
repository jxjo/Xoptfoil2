! MIT License
! Copyright (c) 2022-2024 Jochen Guenzel

module commons

  ! global types and commons 

  implicit none

  ! --- global statics ------------------------------------------------------------ 

  character (*), parameter    :: PGM_NAME = 'Xoptfoil2'

  integer, parameter          :: MODE_NORMAL = 0 
  integer, parameter          :: MODE_CHILD_PROCESS = 1             ! run from eg AirfoilEditor 
  integer                     :: run_mode

  logical                     :: show_details                       ! Show more infos during optimization

  character (*), parameter    :: DESIGN_SUBDIR_POSTFIX = '_temp'
  character (:), allocatable  :: design_subdir                      ! temp directory for all intermediate results
  character (:), allocatable  :: output_prefix                      ! the result output name (of airfoil)


end module commons
