! MIT License
! Copyright (c) 2024 Jochen Guenzel 

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
    integer                   :: nfunctions_top       ! no of control points  
    integer                   :: nfunctions_bot       ! no of control points  
    double precision          :: initial_perturb      ! common max. initial perturb 
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
  public :: hh_get_dv0 
  public :: hh_get_dv_inital_perturb


  ! --- private ---------------------------------------------------
  
  type bound_type 
    double precision              :: min              ! lower boundary  
    double precision              :: max              ! upper boundary 
  end type bound_type


  contains


  subroutine hh_eval_side (hh_specs, x, y)

    !----------------------------------------------------------------------------
    !! evaluates hh functions for airfoil side (top or bot) 
    !
    !    hh_specs:  array of hicks henne specs  
    !    x:   x stations to eval hicks henne
    !    y:   y values to which hicks henne is added
    !----------------------------------------------------------------------------

    type (hh_type), intent(in)     :: hh_specs (:)
    double precision, intent(in)        :: x(:)
    double precision, intent(inout)     :: y(:) 

    double precision, allocatable :: hh_arr (:) 

    integer                       :: j

    do j = 1, size(hh_specs) 

      hh_arr = hh_eval (hh_specs(j), x)
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



  subroutine write_hh_file (filename, top_hh_spec, bot_hh_spec, seed_name, x, y)

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


    character(*),  intent(in)               :: filename, seed_name
    type (hh_spec_type), intent(in)         :: top_hh_spec , bot_hh_spec 
    double precision, allocatable, intent(in) :: x(:), y(:) 

    type (hh_type)      :: hh
    integer             :: iunit, i

    iunit = 13
    open  (unit=iunit, file=filename, status='replace')


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
    !! but prepared for a nice hciks henne distribution  

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
    type (bound_type)             :: strength, location, width
    integer                       :: i, ifunc 
    double precision              :: extent, perturb

    allocate (dv_perturb (nfunctions * 3))

    call hh_bounds (strength, location, width)

    i = 1 
    do ifunc = 1, nfunctions

      ! hicks henne strength 
      extent = abs (strength%max - strength%min)
      perturb = min (0.1d0, extent * initial * 0.5d0)        ! bounds are narrow 
      dv_perturb (i) = perturb             

      ! hicks henne location - equally space between 0 and 1 
      extent = abs (location%max - location%min)
      perturb = min (0.5d0, extent * initial * 1.5d0)          ! let location move around 
      dv_perturb (i+1) = perturb   

      ! hicks henne width = 1 - which is a perfect hicks henne 
      extent = abs (width%max - width%min)
      perturb = min (1d0, extent * initial * 1d0)             ! let width vary 
      dv_perturb (i+2) = perturb               

      i = i + 3
    end do 

    ! print *
    ! print '(A,2F8.4)',"initial       ", initial
    ! print '(A,2F8.4)',"strength      ", strength
    ! print '(A,2F8.4)',"location      ", location
    ! print '(A,2F8.4)',"width         ", width
    ! print '(A,3F8.4)',"dv_perturb    ", dv_perturb (1:3)

  end function


  subroutine hh_bounds (bounds_strength, bounds_location, bounds_width)
  
    !! returns the bounds of hicks henne variables 

    type(bound_type), intent(out)   :: bounds_strength, bounds_location, bounds_width
    
    bounds_strength%min = -0.1d0
    bounds_strength%max =  0.1d0

    bounds_location%min = 0.01d0
    bounds_location%max = 0.99d0
    
    bounds_width%min    = 0.6d0                       ! is some how the reciprocal in hh function
    bounds_width%max    = 3d0                         ! the higher (>1), the smaller the bump 

  end subroutine 

end module