! MIT License
! Copyright (c) 2025 Jochen Guenzel 

module shape_camb_thick
   
  !-------------------------------------------------------------------------
  ! Camb_Thick based airfoil generation, modification, ...
  !
  !     The geometry parameters like thickness, camber etc. are modified
  !     to build a new shape  
  !-------------------------------------------------------------------------
 
  use os_util

  implicit none
  private
 

  ! --- camb_thick types --------------------------------------------------------- 

  type shape_camb_thick_type                              ! describe shaping of an airfoil 
    integer                       :: ndv                  ! number of design variables 
    logical                       :: thickness            ! change max thickness
    logical                       :: thickness_pos        ! change max thickness position 
    logical                       :: camber               ! change max camber
    logical                       :: camber_pos           ! change max camber position 
    logical                       :: le_radius            ! change le radius 
    logical                       :: le_radius_blend      ! change le radius blending distance
    double precision              :: initial_perturb      ! common max. initial perturb 
  end type

  public :: shape_camb_thick_type
  public :: map_dv_to_camb_thick

  ! --- private ---------------------------------------------------
  
  type bound_type 
    double precision              :: min              ! lower boundary  
    double precision              :: max              ! upper boundary 
  end type bound_type


  contains

  subroutine map_dv_to_camb_thick (dv, shape_spec, &
                                  fmaxt, fxmaxt, fmaxc, fxmaxc, fle_radius, le_blend)

    !! maps design variables to a camb_thick specification depending 
    !! if a camp_thick parameter - like 'camber' is activated in shape_spec for optimization

    double precision, intent(in)              :: dv(:)
    type (shape_camb_thick_type), intent(in)  :: shape_spec
    double precision, intent(out)             :: fmaxt, fxmaxt, fmaxc, fxmaxc, fle_radius, le_blend
    integer                 :: idv

    idv = 0 

    ! design variables range from 0..1 - they will be mapped to the specific 
    !     camb_thick shape factors eg ranging from 0.1 ... 10 
    !     -> see airfoil.set_geometry_by_scale
    !
    !     dv    -->   factor 
    !     0.0         min
    !     0.5         1.0         (means "do nothing")
    !     1.0         max


    if (shape_spec%thickness) then 
      idv = idv + 1
      fmaxt = dv_to_factor (dv(idv), 0.02d0, 5d0)  
    else
      fmaxt = 1d0
    end if 

    if (shape_spec%thickness_pos) then 
      idv = idv + 1
      fxmaxt = dv_to_factor (dv(idv), 0.1d0, 1.9d0)  
    else
      fxmaxt = 1d0
    end if 

    if (shape_spec%camber) then 
      idv = idv + 1
      fmaxc = dv_to_factor (dv(idv), 0.01d0, 5d0)  
    else
      fmaxc = 1d0
    end if 

    if (shape_spec%camber_pos) then 
      idv = idv + 1
      fxmaxc = dv_to_factor (dv(idv), 0.1d0, 1.9d0)  
    else
      fxmaxc = 1d0
    end if 

    if (shape_spec%le_radius) then 
      idv = idv + 1
      fle_radius = dv_to_factor (dv(idv), 0.3d0, 3d0)  
    else
      fle_radius = 1d0
    end if 

    if (shape_spec%le_radius_blend) then              ! special case blending distance 
      idv = idv + 1                                   ! which is no factor but the real disnatnce 
      le_blend = 0.01d0 + dv(idv) * (0.5d0 - 0.01d0) 
    else
      le_blend = 0.1d0                                ! default value for blending 
    end if 


  end subroutine 



  function dv_to_factor (dv, f_min, f_max) result (factor) 

    !! maps a design variable to a camb_thick shape factor 
    
    double precision, intent(in)    :: dv, f_min, f_max 
    double precision          :: dv_c, factor

    !     dv    -->   factor 
    !     0.0         min
    !     0.5         1.0         (means "do nothing")
    !     1.0         max

    dv_c = dv - 0.5
    dv_c = min ( 0.5d0, dv_c)
    dv_c = max (-0.5d0, dv_c) 

    if (dv_c == 0d0) then 
      factor = 1d0 
    else if (dv_c < 0d0) then
      factor = 1d0 - 2d0 * abs(dv_c) * (1.0d0 - f_min)
    else
      factor = 1d0 + 2d0 * abs(dv_c) * (f_max - 1d0)
    end if 

  end function 
end module
