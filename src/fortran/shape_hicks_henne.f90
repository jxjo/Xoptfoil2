! MIT License
! Copyright (c) 2024 jxjo

module shape_hicks_henne
   
  !-------------------------------------------------------------------------
  ! Hicks Henne based airfoil generation, modification, ...
  !-------------------------------------------------------------------------
 
  use os_util

  implicit none
  private
 
  ! Hicks Henne general 

  public :: hh_eval
  public :: hh_eval_side

  public :: write_hh_file

  public :: hh_type, hh_spec_type
  
  ! hh and design variables 

  public :: nfunctions_to_ndv
  public :: dv_to_hhs
  public :: hh_dv0 


  ! file function 

  ! hicks henne spec

  type hh_type  
    double precision          :: strength               
    double precision          :: location               
    double precision          :: width               
  end type hh_type

  type hh_spec_type  
    type(hh_type), allocatable :: hhs(:)              !  array of hh definitions
  end type hh_spec_type


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
      if (x(i) >= 0d0 .and. x(i) <= 1d0) then  

        y(i) = st * sin (pi * x(i) **power)**t2

        if (abs(y(i)) < 1d-20) y(i) = 0d0                      ! numerical issues
      else 
        y(i) = 0d0
      end if 
      ! write (*,*) i, x(i), y(i) 
    end do 

  end function



  subroutine write_hh_file (filename, name, top_hh_spec, bot_hh_spec)

    !----------------------------------------------------------------------------
    !! write a hh definitions  to file
    !----------------------------------------------------------------------------

    ! # 'seed airfoil name'
    ! # Top Start
    ! # 0.000strength000000000 0.0000location0000000  0.0000width0000000
    ! # ...
    ! # Top End
    ! # Bottom Start
    ! # ... 
    ! # Bottom End

    character(*),  intent(in)               :: filename, name
    type (hh_spec_type), intent(in)         :: top_hh_spec , bot_hh_spec 

    type (hh_type)     :: hh
    integer                 :: iunit, i

    iunit = 13
    open  (unit=iunit, file=filename, status='replace')

    write (iunit, '(A)') trim(name)
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

    close (iunit)

  end subroutine


  ! ------------- design variables and hh ---------------------------


  subroutine dv_to_hhs (dv, hh_specs)

    !! build array of hicks henne specs out of design variables  
    !! size of dv must be a multiple of 3


    double precision, intent(in)     :: dv(:)
    type (hh_type), allocatable, intent(out) :: hh_specs (:)  

    type (hh_type)   :: hh_spec
    integer               :: i, j
    double precision      :: strength_min, strength_max, location_min, location_max
    double precision      :: width_min, width_max

    ! sanity check 

    if (mod (size(dv),3) /= 0) then 
      call my_stop ("Hicks Henne: wrong number of design variables")
    else
      allocate (hh_specs (int(size(dv)/3)))
    end if 

    ! scale back dv to hh parameters 

    strength_min = -0.005d0
    strength_max =  0.005d0

    location_min = 0.01d0
    location_max = 0.99d0
    
    width_min    = 0.5d0
    width_max    = 5d0                         ! 'reverse' of min_bumb_width 
    
    ! copy dv into hh_spec  

    i = 1 
    do j = 1, size(hh_specs) 
      hh_spec%strength   = dv(i)   * (strength_max - strength_min) + strength_min
      hh_spec%location   = dv(i+1) * (location_max - location_min) + location_min      
      hh_spec%width      = dv(i+2) * (width_max    - width_min   ) + width_min   
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



  function hh_dv0 (nfunctions) result (dv0) 

    !! returns initial design variables resulting in non-pertubed airfoil 

    integer, intent(in)           :: nfunctions 
    double precision, allocatable :: dv0 (:) 

    allocate (dv0 (nfunctions * 3))

    dv0 = 0.5d0                 ! as strength is +- 0.5 this results in strength = 0 = seed airfoil 

  end function
end module