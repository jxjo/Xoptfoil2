! MIT License
! Copyright (c) 2025 Jochen Guenzel 

module shape_hicks_henne
   
  !-------------------------------------------------------------------------
  ! Hicks Henne based airfoil generation, modification, ...
  !-------------------------------------------------------------------------
 
  use os_util

  implicit none
  private

  ! --- hicks henne types --------------------------------------------------------- 

  type shape_hh_type                                  ! describe shaping of an airfoil 
    integer                   :: ndv                  ! number of design variables 
    integer                   :: nfunctions_top       ! no of hh functions  
    integer                   :: nfunctions_bot       ! no of hh functions  
    double precision          :: initial_perturb      ! common max. initial perturb 
    logical                   :: smooth_seed          ! smooth (match bezier) of seed prior to optimization
  end type

  type hh_type                                        ! parms of a single hicks henne function
    double precision          :: strength               
    double precision          :: location               
    double precision          :: width               
  end type hh_type

  type hh_spec_type                                   ! hicks henne functions of one side 
    type(hh_type), allocatable :: hhs(:)              
  end type hh_spec_type

  public :: hh_type, hh_spec_type, shape_hh_type


  ! --- hicks henne functions --------------------------------------------------------- 

  public :: hh_eval
  public :: hh_eval_side
  public :: write_hh_file
  public :: nfunctions_to_ndv
  public :: map_dv_to_hhs
  public :: map_hhs_to_dv
  public :: hh_get_dv0 
  public :: hh_get_dv_inital_perturb
  public :: load_hh_airfoil
  public :: is_hh_file
  public :: print_hh_spec


  ! --- private ---------------------------------------------------
  
  type bound_type 
    double precision              :: min              ! lower boundary  
    double precision              :: max              ! upper boundary 
  end type bound_type


  contains


  subroutine hh_eval_side (hh_spec, x, y)

    !----------------------------------------------------------------------------
    !! evaluates hh functions for airfoil side (top or bot) 
    !
    !    hh_spec:  hh spec which is an array of hh functions   
    !    x:   x stations to eval hicks henne
    !    y:   y values to which hicks henne is added
    !----------------------------------------------------------------------------

    type (hh_spec_type), intent(in)     :: hh_spec
    double precision, intent(in)        :: x(:)
    double precision, intent(inout)     :: y(:) 

    double precision, allocatable :: hh_arr (:) 

    integer                       :: j

    do j = 1, size(hh_spec%hhs) 

      hh_arr = hh_eval (hh_spec%hhs(j), x)
      y = y + hh_arr

    end do 

  end subroutine 


  function hh_eval (hh_spec, x) result (y) 

    !----------------------------------------------------------------------------
    !! Hicks Henne core function - evaluates y (x) for a single Hicks Henne function 
    !!
    !!    hh_spec:  Hicks Henne spec 
    !!    x:    array  0..1 at which to return y Hicks Henne value
    !----------------------------------------------------------------------------

    type (hh_type), intent(in) :: hh_spec
    double precision, intent(in)    :: x (:)
    double precision, allocatable   :: y (:) 
    integer                         :: i 
    double precision                :: t1, t2, st, power, pi

    allocate (y(size(x)))

    t1 = hh_spec%location        
    t2 = hh_spec%width           
    st = hh_spec%strength

    t1 = min (0.999d0, t1)                   ! not too close to bounds (high curvature peeks)
    t1 = max (0.001d0, t1)

    t2 = min (50.0d0, t2)
    t2 = max (0.01d0, t2)

    ! eval Hicks Henne 

    power = log10(0.5d0) / log10(t1)
    pi = acos(-1.d0)

    do i=1, size(x)
      if (x(i) > 0d0 .and. x(i) < 1d0) then                   ! exclude 0.0 and 1.0! 

        y(i) = st * sin (pi * x(i) **power)**t2

        if (abs(y(i)) < 1d-20) y(i) = 0d0                      ! numerical issues
      else 
        y(i) = 0d0
      end if 
      ! write (*,*) i, x(i), y(i) 
    end do 

  end function

  ! ------------- file functions ----------------------------


  function is_hh_file (filename)

    !! .true. if filename has ending '.hicks'

    character(*),  intent(in) :: filename
    logical                   :: is_hh_file 
    character(:), allocatable :: suffix 
    
    suffix = filename_suffix (filename)
    is_hh_file = suffix == '.hicks' .or. suffix =='.HICKS'
    
  end function  



  subroutine write_hh_file (filename, name, top_hh_spec, bot_hh_spec, seed_name, x, y)

    !----------------------------------------------------------------------------
    !! write a hh definitions and seed airfoil coordinates to file
    !----------------------------------------------------------------------------

    ! # Top Start
    ! # 0.000strength000000000 0.0000location0000000  0.0000width0000000
    ! # ...
    ! # Top End
    ! # Bottom Start
    ! # ... 
    ! # Bottom End
    ! # Seedfoil Start 
    ! # 'seed airfoil name'
    ! #  1.000000 0.000000
    ! #  ...      ...


    character(*),  intent(in)               :: filename, name, seed_name
    type (hh_spec_type), intent(in)         :: top_hh_spec , bot_hh_spec 
    double precision, allocatable, intent(in) :: x(:), y(:) 

    type (hh_type)      :: hh
    integer             :: iunit, i

    iunit = 13
    open  (unit=iunit, file=filename, status='replace')

    ! airfoil name 

    write (iunit, '(A)') trim(name)

    ! hh function values 

    write (iunit, '(A)') "Top Start"
    do i = 1, size(top_hh_spec%hhs)
      hh = top_hh_spec%hhs (i) 
      write (iunit, '(3F14.10)') hh%strength, hh%location, hh%width
    end do 
    write (iunit, '(A)') "Top End"

    write (iunit, '(A)') "Bottom Start"
    do i = 1, size(bot_hh_spec%hhs)
      hh = bot_hh_spec%hhs (i) 
      write (iunit, '(3F14.10)') hh%strength, hh%location, hh%width
    end do 
    write (iunit, '(A)') "Bottom End"

    ! seed airfoil where hh functions are applied 

    write (iunit, '(A)') "Seedfoil Start"
    write (iunit, '(A)') trim(seed_name)

    do i = 1, size(x) 
      write(iunit,'(2F12.7)') x(i), y(i)
    end do 

    close (iunit)

  end subroutine



  subroutine load_hh_airfoil (filename, name, top_hh_spec, bot_hh_spec, seed_name, x, y)

    !----------------------------------------------------------------------------
    !! read a hh definitions and seed airfoil coordinates from file
    !----------------------------------------------------------------------------

    ! # 'airfoil name'
    ! # Top Start
    ! # 0.000strength000000000 0.0000location0000000  0.0000width0000000
    ! # ...
    ! # Top End
    ! # Bottom Start
    ! # ... 
    ! # Bottom End
    ! # Seedfoil Start 
    ! # 'seed airfoil name'
    ! #  1.000000 0.000000
    ! #  ...      ...

    character(*),  intent(in)                   :: filename
    character(:), allocatable, intent(out)      :: name 
    character(:), allocatable, intent(out)      :: seed_name
    type (hh_spec_type), intent(out)            :: top_hh_spec , bot_hh_spec 
    double precision, allocatable, intent(out)  :: x(:), y(:) 

    type (hh_type), dimension (100)     :: hh_tmp
    double precision, dimension (1000)  :: x_tmp, y_tmp
    character (255)           :: in_buffer 
    character(:), allocatable :: in_line
    integer                   :: iunit, i, nhh, nc, ioerr
    logical                   :: do_read

    ! Open hh definition file
  
    iunit = 12
    open(unit=iunit, file=filename, status='old', position='rewind', iostat=ioerr)
    if (ioerr /= 0) then
      write (*,*) 'Cannot find hicks-henne definition file '//trim(filename)
      stop 1
    end if
   
    ! Read name

    read(iunit, '(A)',iostat=ioerr) in_buffer
    name = trim(in_buffer)

    ! Read top hh specs 
  
    do_read = .false. 
    nhh = 0

    do i = 1, size(hh_tmp) 
      read(iunit, '(A)',iostat=ioerr) in_buffer
      in_line = trim(in_buffer) 
      if (in_line == 'Top Start') then
        do_read = .true. 
      else if (do_read .and. (in_line == 'Top End')) then 
        exit  
      else if (do_read) then 
        nhh = nhh + 1
        read (in_line,*) hh_tmp(nhh)%strength, hh_tmp(nhh)%location, hh_tmp(nhh)%width
      else 
        call my_stop ('Syntax error in hicks-henne definition file '//trim(filename)//' (Top)')
      end if
    end do

    top_hh_spec%hhs = hh_tmp (1:nhh)

    ! Read bot hh specs 
  
    do_read = .false. 
    nhh = 0

    do i = 1, size(hh_tmp) 
      read(iunit, '(A)',iostat=ioerr) in_buffer
      in_line = trim(in_buffer) 
      if (in_line == 'Bottom Start') then
        do_read = .true. 
      else if (do_read .and. (in_line == 'Bottom End')) then 
        exit  
      else if (do_read) then 
        nhh = nhh + 1
        read (in_line,*) hh_tmp(nhh)%strength, hh_tmp(nhh)%location, hh_tmp(nhh)%width
      else 
        call my_stop ('Syntax error in hicks-henne definition file '//trim(filename)//' (Bottom)')
      end if
    end do

    bot_hh_spec%hhs = hh_tmp (1:nhh)

    ! Read seed foil name  
  
    read(iunit, '(A)',iostat=ioerr) in_buffer

    if (trim(in_buffer) /= "Seedfoil Start") then 
      call my_stop ('Syntax error in hicks-henne definition file '//trim(filename)//' (Seedfoil)')
    end if 

    read(iunit, '(A)',iostat=ioerr) in_buffer
    seed_name = trim(in_buffer)

    ! Read seed foil coordinates   

    nc = 0 
    do i = 1, size(x_tmp) 

      read(iunit, '(A)',iostat=ioerr) in_buffer

      if (ioerr /= 0) exit                                    ! end of file 
      if (len(trim(in_buffer)) == 0)  exit                    ! empty line (at end) 

      nc = nc + 1
      read (in_buffer,*) x_tmp(nc), y_tmp(nc)
      x_tmp(nc) = x_tmp(nc) + 0d0                             ! get rid of -0d0
      y_tmp(nc) = y_tmp(nc) + 0d0 

    end do 

    if (nc == 0) then 
      call my_stop ('Syntax error in hicks-henne definition file '//trim(filename)//' (Coordinates)')
    end if 

    x = x_tmp(1:nc)
    y = y_tmp(1:nc)

    close (iunit)

  end subroutine


  ! ------------- design variables and hh ---------------------------


  subroutine map_dv_to_hhs (dv, hh_specs)

    !! build array of hicks henne specs out of design variables  
    !! size of dv must be a multiple of 3


    double precision, intent(in)     :: dv(:)
    type (hh_type), allocatable, intent(out) :: hh_specs (:)  

    type (hh_type)        :: hh_spec
    type (bound_type)     :: strength, location, width
    integer               :: i, j

    ! sanity check 

    if (mod (size(dv),3) /= 0) then 
      call my_stop ("Hicks Henne: wrong number of design variables")
    else
      allocate (hh_specs (int(size(dv)/3)))
    end if 

    ! scale back dv to hh parameters 

    call hh_bounds (strength, location, width)
    
    ! copy dv into hh_spec  

    i = 1 
    do j = 1, size(hh_specs) 
      hh_spec%strength   = dv(i)   * (strength%max - strength%min) + strength%min
      hh_spec%location   = dv(i+1) * (location%max - location%min) + location%min      
      hh_spec%width      = dv(i+2) * (width%max    - width%min   ) + width%min   
      i = i + 3
      hh_specs(j) = hh_spec
      ! print *, hh_spec
    end do 

  end subroutine



  function map_hhs_to_dv (hhs) result (dv) 

    !! returns design variables out of hicks henne functions 
    !! but prepared for a nice hicks henne distribution  

    type(hh_type), intent(in)           :: hhs (:) 

    double precision, allocatable :: dv (:) 
    type (bound_type)     :: strength, location, width
    integer               :: i, ifunc, nfunctions

    nfunctions = size (hhs) 
    allocate (dv (nfunctions * 3))

    call hh_bounds (strength, location, width)

    i = 1 
    do ifunc = 1, nfunctions

      ! hicks henne strength = 0 - equals seed
      dv (i)    = (hhs(ifunc)%strength - strength%min) / abs (strength%max - strength%min)

      ! hicks henne location - equally space between 0 and 1 
      dv (i+1)  = (hhs(ifunc)%location - location%min) / abs (location%max - location%min)

      ! hicks henne width = 1 - which is a perfect hicks henne 
      dv (i+2)  = (hhs(ifunc)%width - width%min) / abs (width%max - width%min)

      i = i + 3
    end do 

  end function



  function nfunctions_to_ndv (nfunctions_top, nfunctions_bot)
    !! get number of design variables from number of hh functions top and bot 
    integer, intent(in)           :: nfunctions_top
    integer, intent(in), optional :: nfunctions_bot
    integer             :: nfunctions_to_ndv

    if (present (nfunctions_bot)) then
      nfunctions_to_ndv = nfunctions_top * 3  + nfunctions_bot * 3
    else
      nfunctions_to_ndv = nfunctions_top * 3  
    end if 

  end function



  function hh_get_dv0 (nfunctions) result (dv0) 

    !! returns initial design variables resulting in non-pertubed airfoil
    !! but prepared for a nice hicks henne distribution  

    integer, intent(in)           :: nfunctions 

    double precision, allocatable :: dv0 (:) 
    type (bound_type)     :: strength, location, width
    integer               :: i, ifunc 
    double precision      :: loc 

    allocate (dv0 (nfunctions * 3))

    call hh_bounds (strength, location, width)

    i = 1 
    do ifunc = 1, nfunctions

      ! hicks henne strength = 0 - equals seed
      dv0 (i) = (0d0 - strength%min) / abs (strength%max - strength%min)

      ! hicks henne location - equally space between 0 and 1 
      loc = (1d0/(nfunctions+1)) * ifunc
      dv0 (i+1) = (loc - location%min) / abs (location%max - location%min)

      ! hicks henne width = 1 - which is a perfect hicks henne 
      dv0 (i+2) = (1d0 - width%min) / abs (width%max - width%min)

      i = i + 3
    end do 

  end function



  function hh_get_dv_inital_perturb (initial, nfunctions) result (dv_perturb) 

    !! returns initial perturb of design variables depending on
    !!     strength, location, width
    !!     (the common initial value is defined in inputs)  

    double precision, intent(in)  :: initial 
    integer, intent(in)           :: nfunctions 

    double precision, allocatable :: dv_perturb (:) 
    integer                       :: i, ifunc 

    allocate (dv_perturb (nfunctions * 3))

    i = 1 
    do ifunc = 1, nfunctions

      ! hicks henne strength 
      dv_perturb (i)  = min (0.1d0, initial * 0.25d0)                     ! strength more careful 

      ! hicks henne location - equally space between 0 and 1 
      dv_perturb (i+1) = min (0.5d0, initial * 1.0d0)                     ! let location move around 

      ! hicks henne width = 1 - which is a perfect hicks henne 
      dv_perturb (i+2) = min (0.3d0, initial * 1.0d0)                        ! let width vary 

      i = i + 3
    end do 

    ! print *
    ! print '(A,2F8.4)',"initial       ", initial
    ! print '(A,2F8.4)',"strength      ", strength
    ! print '(A,2F8.4)',"location      ", location
    ! print '(A,2F8.4)',"width         ", width
    ! print '(A,100F8.4)',"dv_perturb    ", dv_perturb 

  end function


  subroutine hh_bounds (bounds_strength, bounds_location, bounds_width)
  
    !! returns the bounds of hicks henne variables 

    type(bound_type), intent(out)   :: bounds_strength, bounds_location, bounds_width
    
    bounds_strength%min = -0.02d0
    bounds_strength%max =  0.02d0

    bounds_location%min = 0.01d0
    bounds_location%max = 0.99d0
    
    bounds_width%min    = 0.7d0 ! 0.6d0                       ! is some how the reciprocal in hh function
    bounds_width%max    = 4d0   ! 3d0                         ! the higher (>1), the smaller the bump 

  end subroutine 



  subroutine print_hh_spec (ip, side, hh_spec)

    !----------------------------------------------------------------------------
    !! debug: print hh definitions 
    !----------------------------------------------------------------------------

    integer, intent(in)                     :: ip
    character(*),  intent(in)               :: side 
    type (hh_spec_type), intent(in)         :: hh_spec 

    type (hh_type)      :: hh
    integer             :: i

    write (*,'(I2,A)', advance='no') ip, " "//side // ":  "
    do i = 1, size(hh_spec%hhs)
      hh = hh_spec%hhs (i) 
      write (*,'(3F7.4,"   ")', advance='no') hh%strength, hh%location, hh%width
    end do 
    write (*,*) 

  end subroutine



end module