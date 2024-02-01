! MIT License
! Copyright (C) 2017-2019 Daniel Prosser
! Copyright (c) 2024 Jochen Guenzel 

module shape_airfoil

  ! Create an airfoil shape from design variables

  use os_util
  use commons,        only: airfoil_type
 
  implicit none
  private
 
  ! Common types 

  type shape_bezier_type
    integer                       :: ndv                  ! number of design variables 
    integer                       :: ncp_top              ! no of control points  
    integer                       :: ncp_bot              ! no of control points  
    double precision              :: initial_perturb      ! common max. initial perturb
  end type


  type shape_hh_type
    integer                       :: ndv                  ! number of design variables 
    integer                       :: nfunctions_top       ! no of control points  
    integer                       :: nfunctions_bot       ! no of control points  
    double precision              :: initial_perturb      ! common max. initial perturb 
    double precision              :: min_width            ! is some how the reciprocal in hh function
    double precision              :: max_width            ! the higher (>1), the smaller the bump 
  end type
 

  type shape_camb_thick_type
    integer                       :: ndv = 6              ! fixed for thickness, camber, ....  
  end type

  type shape_spec_type
    integer                       :: type                 ! HICKS_HENNE or BEZIER or ...
    integer                       :: ndv                  ! number of design variables 
    type(shape_hh_type)           :: hh
    type(shape_bezier_type)       :: bezier
    type(shape_camb_thick_type)   :: camb_thick
  end type

  ! --- Public ------------------------------------------------------- 

  ! types 

  public          :: shape_spec_type
  public          :: shape_bezier_type, shape_hh_type

  ! enums 

  integer, parameter, public  :: HICKS_HENNE = 1          ! shape types 
  integer, parameter, public  :: BEZIER      = 2
  integer, parameter, public  :: CAMB_THICK  = 3

  ! functions 

  public          :: set_shape_spec
  public          :: set_seed_foil, get_seed_foil
  public          :: get_ndv_of_shape
  public          :: get_dv0_of_shape
  public          :: get_dv_initial_perturb_of_shape
  public          :: create_airfoil_bezier
  public          :: create_airfoil_camb_thick
  public          :: create_airfoil_hicks_henne
  public          :: smooth_foil

  ! variables 

  type (shape_spec_type), public    :: shape_spec           ! main shape specification, static 


  ! ---- private, static -------------------------------------------------------
  
  type (airfoil_type)            :: seed_foil


contains

  subroutine set_shape_spec (shape_spec_in)

    !-----------------------------------------------------------------------------
    !! sets spahe_spec into static mod variable for later evaluation  
    !-----------------------------------------------------------------------------

    type (shape_spec_type), intent(in)  :: shape_spec_in 

    shape_spec = shape_spec_in

  end subroutine 



  subroutine set_seed_foil (seed_foil_in)

    !-----------------------------------------------------------------------------
    !! sets seed_foil into static mod variable for later evaluation  
    !-----------------------------------------------------------------------------

    use commons,                only : airfoil_type
    type (airfoil_type), intent(in)  :: seed_foil_in 

    seed_foil = seed_foil_in

  end subroutine 



  subroutine get_seed_foil (seed_foil_out)

    !-----------------------------------------------------------------------------
    !! gets seed_foil from  static mod variable 
    !-----------------------------------------------------------------------------

    use commons,                only : airfoil_type
    type (airfoil_type), intent(out)  :: seed_foil_out 

    seed_foil_out = seed_foil

  end subroutine 



  subroutine create_airfoil_hicks_henne (dv, foil)

    !-----------------------------------------------------------------------------
    !! Creates an airfoil surface by perturbing an input "seed" airfoil
    !! with Hicks-Henne shape functions
    !-----------------------------------------------------------------------------

    use commons,                only : airfoil_type
    use airfoil_operations,     only : rebuild_from_sides
    use shape_hicks_henne,      only : nfunctions_to_ndv, hh_type, map_dv_to_hhs, hh_eval_side

    double precision, intent(in)    :: dv(:) 
    type(airfoil_type), intent(out) :: foil 

    double precision, allocatable   :: dv_top (:), dv_bot (:)
    integer                         :: ndv_top

    type (hh_type), allocatable     :: top_hh_specs (:) , bot_hh_specs (:)  
    double precision, allocatable   :: yt_new(:), yb_new(:)


    ! top side - design variables to hh functions 

    ndv_top = nfunctions_to_ndv (shape_spec%hh%nfunctions_top)
    dv_top = dv (1: ndv_top)

    call map_dv_to_hhs (dv_top, top_hh_specs)                       ! rebuild hicks henne specs 
    yt_new = seed_foil%top%y
    call hh_eval_side (top_hh_specs, seed_foil%top%x, yt_new )  ! and add hicks hennes to y 

    ! bot side - design variables to hh functions 

    if (.not. seed_foil%symmetrical) then

      dv_bot = dv (ndv_top + 1 : )

      call map_dv_to_hhs (dv_bot, bot_hh_specs)
      yb_new = seed_foil%bot%y                                  ! initial y value 
      call hh_eval_side (bot_hh_specs, seed_foil%bot%x, yb_new )
  
    else

      yb_new = -yt_new

    end if


    foil%top%x  = seed_foil%top%x
    foil%top%y  = yt_new
    foil%bot%x  = seed_foil%bot%x
    foil%bot%y  = yb_new

    call rebuild_from_sides (foil%top, foil%bot, foil)

    ! !$omp critical
    !     if (ndv_top > 0) then 
    !       print '(A,F10.5,F12.6)', "## thickness", maxval(foil%y) * 100d0, dv (1)
    !     else
    !       print *, "bot", dv (1), bot_hh_specs(1)
    !     end if 
    ! !$omp end critical
    foil%is_hh_based  = .true.
    foil%hh_seed_name = seed_foil%name
    foil%top_hh%hhs   = top_hh_specs                           ! could be useful to keep 
    foil%bot_hh%hhs   = bot_hh_specs         

  end subroutine create_airfoil_hicks_henne



  subroutine create_airfoil_bezier (dv, foil) 

    !-----------------------------------------------------------------------------
    !! Create airfoil from bezier design variables
    !!  - seed is only needed to determine TE gap and number of points 
    !!  - design variables dv are converted back to bezier control points 
    !!    to generate bezier curve 
    !-----------------------------------------------------------------------------

    use commons,                 only : airfoil_type
    use airfoil_operations,     only : split_foil_at_00_into_sides, te_gap 
    use shape_bezier,           only : bezier_spec_type
    use shape_bezier,           only : ncp_to_ndv_side
    use shape_bezier,           only : map_dv_to_bezier, bezier_eval_airfoil

    double precision,  intent(in)   :: dv (:)
    type(airfoil_type), intent(out) :: foil 

    double precision, allocatable   :: dv_top (:), dv_bot (:)
    type(bezier_spec_type)  :: top_bezier, bot_bezier 
    double precision        :: side_te_gap
    integer                 :: ndv_top

    ! retrieve design variables for top and bot and rebuild bezier control points

    side_te_gap = te_gap (seed_foil) / 2

    ndv_top = ncp_to_ndv_side (shape_spec%bezier%ncp_top)
    dv_top = dv (1: ndv_top)
    call map_dv_to_bezier ('Top', dv_top, side_te_gap, top_bezier)

    if (.not. seed_foil%symmetrical) then
      dv_bot = dv (ndv_top + 1 : )
      call map_dv_to_bezier ('Bot', dv_bot, side_te_gap, bot_bezier)
    else 
      bot_bezier = top_bezier
      bot_bezier%py = - top_bezier%py
    end if 

    ! build airfoil with control points 

    call bezier_eval_airfoil (top_bezier, bot_bezier, seed_foil%npoint, foil%x, foil%y) 
        ! !$omp critical
        !     write (*,*) 
        !     write (*,*) "top ", top_bezier%px
        !     write (*,*) "top ", top_bezier%py
        !     write (*,*) "bot ", bot_bezier%px
        !     write (*,*) "bot ", bot_bezier%py
        ! !$omp end critical

    foil%npoint = size(foil%x)
    foil%is_bezier_based = .true.
    foil%top_bezier  = top_bezier        ! could be useful to keep 
    foil%bot_bezier  = bot_bezier         

    call split_foil_at_00_into_sides (foil)

  end subroutine create_airfoil_bezier 



  subroutine create_airfoil_camb_thick (dv, foil)

    !-----------------------------------------------------------------------------
    !! Modify thickness and camber and their positions of 
    !!   the seed foil defined by xt_seed, zt_seed, xb_seed, zb_seed
    !!
    !! Returns the new foil defined by zt_new, zb_new
    !-------------------------------------------------------------------------------

    use commons,             only : airfoil_type
    use xfoil_driver,       only : xfoil_scale_thickness_camber, xfoil_scale_LE_radius
                                    
    double precision,  intent(in)   :: dv (:)
    type(airfoil_type), intent(out) :: foil
    
    type(airfoil_type) :: new_foil_1, new_foil_2
    double precision :: f_thick,d_xthick,f_camb,d_xcamb
    double precision :: f_radius, x_blend 


    ! Change thickness, camber ... according to new values hidden in dv

    f_camb   = 1.d0 + 10.d0 * dv(1) 
    f_thick  = 1.d0 + 5.d0 * dv(2)
    d_xcamb  = 4.d0 * dv(3)
    d_xthick = 4.d0 * dv(4)

    call xfoil_scale_thickness_camber (seed_foil, f_thick,d_xthick,f_camb,d_xcamb, new_foil_1)

    ! Change LE radius ... according to new values hidden in dv

    f_radius = 1.d0 + 3.d0 * dv(5)
    x_blend  = max (0.02d0, (5.d0 * dv(6) + 0.1d0))
    call xfoil_scale_LE_radius (new_foil_1, f_radius, x_blend, new_foil_2)
    
    ! Especially Xfoils HIPNT tends to produce artefacts in curvature
    ! Smoothing should also be done for the seed airfoil 
    
    call smooth_foil (.false., 0.05d0, new_foil_2)

    ! Sanity check - new_foil may not have different number of points
    
    if (seed_foil%npoint /= new_foil_2%npoint) then
      call my_stop ('Number of points changed during thickness/camber modification')
    end if

    foil = new_foil_2
    
  end subroutine create_airfoil_camb_thick


  
  function get_ndv_of_shape () result (ndv)

    !----------------------------------------------------------------------------
    !! no of designvars of shape depending on shape type
    !----------------------------------------------------------------------------
  
    integer :: ndv
  
    if (shape_spec%type == CAMB_THICK) then    
      ndv = shape_spec%camb_thick%ndv
    else if (shape_spec%type == BEZIER) then
      ndv = shape_spec%bezier%ndv
    else if (shape_spec%type == HICKS_HENNE) then                                      
      ndv = shape_spec%hh%ndv
    else 
      ndv = 0 
    end if
  
  end function 



  function get_dv0_of_shape () result (dv0)

    !----------------------------------------------------------------------------
    !! designvars for design 0 (equals seed foil) depending on shape type
    !----------------------------------------------------------------------------
  
    use shape_hicks_henne,      only : hh_get_dv0
    use shape_bezier,           only : bezier_get_dv0


    double precision, allocatable :: dv0 (:) 
    integer                       :: ntop, nbot
  
    ! Set initial design
  
    if (shape_spec%type == CAMB_THICK) then    
  
      ! dv is either scale ( 1+ dv) or delta x  highpoint    
      allocate (dv0 (shape_spec%camb_thick%ndv))
      dv0 = 0.5d0                                       ! equals no change to seed                                            
  
    else if (shape_spec%type == BEZIER) then
  
      ! take the bezier definition of seed airfoil as dv0 
      dv0 = [bezier_get_dv0 ("Top", seed_foil%top_bezier), &
             bezier_get_dv0 ("Bot", seed_foil%bot_bezier)]
  
    else if (shape_spec%type == HICKS_HENNE) then                                      
      
      ! concat dv0 of top and bot side hicks-henne functions 
      ntop = shape_spec%hh%nfunctions_top
      nbot = shape_spec%hh%nfunctions_bot
      dv0 = [hh_get_dv0 (ntop), hh_get_dv0 (nbot)]
  
    else 
  
      call my_stop ("Unknown shape type")
  
    end if
  
  end function 
  
  

  function get_dv_initial_perturb_of_shape () result (dv_perturb)

    !----------------------------------------------------------------------------
    !! inital max. perturb of the designvars for design 0 depending on shape type
    !----------------------------------------------------------------------------
  
    use shape_hicks_henne,      only : hh_get_dv_inital_perturb
    use shape_bezier,           only : bezier_get_dv_inital_perturb

    double precision, allocatable :: dv_perturb (:)
    integer                       :: ntop, nbot
    double precision              :: initial
  
    ! Set initial design
  
    if (shape_spec%type == CAMB_THICK) then    
  
      ! dv is either scale ( 1+ dv) or delta x  highpoint    
      allocate (dv_perturb (shape_spec%camb_thick%ndv))
      dv_perturb = 0.05d0                    ! equals 5% of design space                                           
  
    else if (shape_spec%type == BEZIER) then
  
      initial = shape_spec%bezier%initial_perturb
      dv_perturb = [ bezier_get_dv_inital_perturb (initial, "Top", seed_foil%top_bezier), &
                     bezier_get_dv_inital_perturb (initial, "Bot", seed_foil%bot_bezier)]
  
    else if (shape_spec%type == HICKS_HENNE) then                                      
      
      ! concat dv0 of top and bot side hicks-henne functions 
      ntop = shape_spec%hh%nfunctions_top
      nbot = shape_spec%hh%nfunctions_bot
      initial = shape_spec%hh%initial_perturb

      dv_perturb = [hh_get_dv_inital_perturb (initial, ntop), &
                    hh_get_dv_inital_perturb (initial, nbot)]
  
    else 
  
      call my_stop ("Unknown shape type")
  
    end if
  
  end function 
  

  
  subroutine smooth_foil (show_details, spike_threshold, foil)

    !-------------------------------------------------------------------------------------
    !! Smooth an airfoil with its coordinate in foil%x and foil%y
    !!
    !! Returns  the smoothed airfoil  
    !!
    !! *** this subroutine is in this module because of module hierarchy ***
    !-------------------------------------------------------------------------------------

    use commons,             only : airfoil_type
    use math_deps,          only : smooth_it
    use airfoil_operations, only : rebuild_from_sides
  
    logical, intent(in)               :: show_details  
    double precision, intent(in)      :: spike_threshold
    type(airfoil_type), intent(inout) :: foil

    call smooth_it (show_details, spike_threshold, foil%top%x, foil%top%y)
    call smooth_it (show_details, spike_threshold, foil%bot%x, foil%bot%y)

    call rebuild_from_sides (foil%top, foil%bot, foil)

  end subroutine smooth_foil





  subroutine write_dv_as_shape_data (step, iparticle, dv)

    !------------------------------------------------------------------------------
    !! Analysis: Write design variabales either as bezier or hicks henne to dump csv file 
    !------------------------------------------------------------------------------

    use commons,            only : design_subdir
    use shape_bezier,       only : bezier_spec_type
    use shape_bezier,       only : ncp_to_ndv_side, map_dv_to_bezier, write_bezier_file

    integer, intent(in)           :: step, iparticle
    double precision, intent(in)  :: dv (:) 

    double precision, allocatable   :: dv_shape_spec (:), dv_top(:), dv_bot(:)
    type(bezier_spec_type)          :: top_bezier, bot_bezier
    character (:), allocatable      :: dump_file, dump_name
    integer     :: ndv_top
    ! integer     :: i, iunit, stat, ndv_top

    ! Open dump file - either create new or append 

    dump_name = "dump_dv-"//stri(step)//"-"//stri(iparticle)

    ! open(unit=iunit, iostat=stat, file=dump_file, status='old')
    ! if (stat == 0) then 
    !   close(iunit) 
    !   print *,"before"
    !   open (unit=iunit, file=dump_file, status='old', position='append', action = 'readwrite', err=901)
    !   print *,"after"
    ! else 
    !   open (unit=iunit, file=dump_file, status='replace', action = 'readwrite', err=901)
    ! end if 

!$OMP CRITICAL
    if (shape_spec%type == BEZIER) then
        
      ! dv to bezier shape paramters 

      dv_shape_spec = dv (1 : shape_spec%bezier%ndv)

      ndv_top = ncp_to_ndv_side (shape_spec%bezier%ncp_top)
      dv_top = dv_shape_spec (1: ndv_top)
      call map_dv_to_bezier ('Top', dv_top, 0d0, top_bezier)

      dv_bot = dv_shape_spec (ndv_top + 1 : )
      call map_dv_to_bezier ('Bot', dv_bot, 0d0, bot_bezier)

      ! #todo handel symmetrical 
        ! if (.not. seed_foil%symmetrical) then
        !   dv_bot = dv_shape_spec (ndv_top + 1 : )
        !   call map_dv_to_bezier ('Bot', dv_bot, 0d0, bot_bezier)
        ! else 
        !   bot_bezier = top_bezier
        !   bot_bezier%py = - top_bezier%py
        ! end if 

      dump_file = design_subdir//dump_name//'.bez'
      call write_bezier_file (dump_file, dump_name, top_bezier, bot_bezier)
      ! write bezier data 
    
      ! write (iunit, '(I5,";",I5,";",A5)', advance='no') step, iparticle, 'Top'
      ! do i = 1,size(top_bezier%px)
      !   write (iunit, '(2(";",F12.8))', advance='no') top_bezier%px(i), top_bezier%py(i)
      ! end do 
      ! write (iunit,*)
    
      ! write (iunit, '(I5,";",I5,";",A5)', advance='no') step, iparticle, 'Bot'
      ! do i = 1,size(bot_bezier%px)
      !   write (iunit, '(2(";",F12.8))', advance='no') bot_bezier%px(i), bot_bezier%py(i)
      ! end do 
      ! write (iunit,*)
    

    else 

      dv_shape_spec = dv (1 : shape_spec%hh%ndv)

      call my_stop ("dump of hicks henne dv not implemented")

    end if
!$OMP END CRITICAL   

    return 

  ! 901 call print_warning ("Warning: unable to open "//dump_file//". Skipping ...")
  ! return
  end subroutine

end module shape_airfoil