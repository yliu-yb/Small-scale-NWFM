!------------------------------------------------------------------------------
! Institution, Affiliation
!------------------------------------------------------------------------------
!
! MODULE:  Module name
!
!> @author
!> yan liu}
!
! DESCRIPTION: 
!>  IO module need grid and time config in the data module initialniezd first
!
! REVISION HISTORY:
! 27 09 2022 - Initial Version
! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
!------------------------------------------------------------------------------

module IO
    use netcdf
    use data
    implicit none
    integer :: ncid_out, ncid_force

    ! model output nc variable
    integer :: lvl_dimid, lon_dimid, lat_dimid, t_dimid
    integer :: lon_varid, lat_varid, t_varid
    integer :: density_varid, u_varid, v_varid, w_varid, theta_varid, pres_varid, height_varid
    
    ! boundary nc variable id
    integer :: density_b_varid, u_b_varid, v_b_varid, w_b_varid, theta_b_varid, pres_b_varid
    integer :: density_t_varid, u_t_varid, v_t_varid, w_t_varid, theta_t_varid, pres_t_varid
    integer :: density_w_varid, u_w_varid, v_w_varid, w_w_varid, theta_w_varid, pres_w_varid
    integer :: density_e_varid, u_e_varid, v_e_varid, w_e_varid, theta_e_varid, pres_e_varid
    integer :: density_n_varid, u_n_varid, v_n_varid, w_n_varid, theta_n_varid, pres_n_varid
    integer :: density_s_varid, u_s_varid, v_s_varid, w_s_varid, theta_s_varid, pres_s_varid
    real, dimension(:), allocatable :: force_dt
    
    integer, parameter :: NDIMS = 4
    integer :: dimids(NDIMS)

    character (len = *), parameter :: TIME_NAME = "times"
    character (len = *), parameter :: LVL_NAME = "levels"
    character (len = *), parameter :: LAT_NAME = "latitudes"
    character (len = *), parameter :: LON_NAME = "longitudes"

    character (len = *), parameter :: DENSITY_NAME = "density"
    character (len = *), parameter :: U_NAME = "U"
    character (len = *), parameter :: V_NAME = "V"
    character (len = *), parameter :: W_NAME = "W"
    character (len = *), parameter :: THETA_NAME = "theta"
    character (len = *), parameter :: PRES_NAME = "pressure"
    character (len = *), parameter :: HEIGHT_NAME = "height"

    character (len = *), parameter :: UNITS = "units"
    character (len = *), parameter :: DENSITY_UNITS = "kg m-3"
    character (len = *), parameter :: U_UNITS = "ms-1"
    character (len = *), parameter :: V_UNITS = "ms-1"
    character (len = *), parameter :: W_UNITS = "ms-1"
    character (len = *), parameter :: THETA_UNITS = ""
    character (len = *), parameter :: PRES_UNITS = "hPa"
    character (len = *), parameter :: HEIGHT_UNITS = "m"
    character (len = *), parameter :: LAT_UNITS = "degrees_north"
    character (len = *), parameter :: LON_UNITS = "degrees_east"
    character (len = *), parameter :: TIME_UNITS = "seconds since 1970-01-01 00:00:00"

    character (len = *), parameter :: DENSITY_bottom_NAME = "density_bottom"
    character (len = *), parameter :: DENSITY_top_NAME = "density_top"
    character (len = *), parameter :: DENSITY_west_NAME = "density_west"
    character (len = *), parameter :: DENSITY_east_NAME = "density_east"
    character (len = *), parameter :: DENSITY_south_NAME = "density_south"
    character (len = *), parameter :: DENSITY_north_NAME = "density_north"
    
    character (len = *), parameter :: TH_bottom_NAME = "theta_bottom"
    character (len = *), parameter :: TH_top_NAME    = "theta_top"
    character (len = *), parameter :: TH_west_NAME   = "theta_west"
    character (len = *), parameter :: TH_east_NAME   = "theta_east"
    character (len = *), parameter :: TH_south_NAME  = "theta_south"
    character (len = *), parameter :: TH_north_NAME  = "theta_north"

    character (len = *), parameter :: U_bottom_NAME = "U_bottom"
    character (len = *), parameter :: U_top_NAME    = "U_top"
    character (len = *), parameter :: U_west_NAME   = "U_west"
    character (len = *), parameter :: U_east_NAME   = "U_east"
    character (len = *), parameter :: U_south_NAME  = "U_south"
    character (len = *), parameter :: U_north_NAME  = "U_north"

    character (len = *), parameter :: V_bottom_NAME = "V_bottom"
    character (len = *), parameter :: V_top_NAME    = "V_top"
    character (len = *), parameter :: V_west_NAME   = "V_west"
    character (len = *), parameter :: V_east_NAME   = "V_east"
    character (len = *), parameter :: V_south_NAME  = "V_south"
    character (len = *), parameter :: V_north_NAME  = "V_north"

    character (len = *), parameter :: W_bottom_NAME = "W_bottom"
    character (len = *), parameter :: W_top_NAME    = "W_top"
    character (len = *), parameter :: W_west_NAME   = "W_west"
    character (len = *), parameter :: W_east_NAME   = "W_east"
    character (len = *), parameter :: W_south_NAME  = "W_south"
    character (len = *), parameter :: W_north_NAME  = "W_north"

    character (len = *), parameter :: P_bottom_NAME = "P_bottom"
    character (len = *), parameter :: P_top_NAME    = "P_top"
    character (len = *), parameter :: P_west_NAME   = "P_west"
    character (len = *), parameter :: P_east_NAME   = "P_east"
    character (len = *), parameter :: P_south_NAME  = "P_south"
    character (len = *), parameter :: P_north_NAME  = "P_north"
    
    ! The start and count arrays will tell the netCDF library where to write data.
    integer :: start(NDIMS), count(NDIMS)

contains
    subroutine create_nc()
        ! create nc to save prognostic data
        ! define nc dimension, variales and units
        character (len = *), parameter :: FILE_NAME = "NWFM_out_test.nc"
        integer :: NTIMES, NLVLS, NLATS, NLONS
        integer :: i

        NTIMES = time_para%run_hours * 3600 / time_para%t_interval
        NLVLS = grid_para%z_nums
        NLATS = grid_para%y_nums
        NLONS = grid_para%x_nums
        
        call check( nf90_create(path=FILE_NAME, cmode=or(nf90_clobber,nf90_64bit_offset), ncid=ncid_out) )
        ! define the dimensions
        call check( nf90_def_dim(ncid_out, TIME_NAME, NTIMES, t_dimid) )
        call check( nf90_def_dim(ncid_out, LVL_NAME, NLVLS, lvl_dimid) )
        call check( nf90_def_dim(ncid_out, LAT_NAME, NLATS, lat_dimid) )
        call check( nf90_def_dim(ncid_out, LON_NAME, NLONS, lon_dimid) )
        
        ! define coordinate variables
        call check( nf90_def_var(ncid_out, TIME_NAME, NF90_INT, t_dimid, t_varid) )
        call check( nf90_def_var(ncid_out, LAT_NAME, NF90_REAL, lat_dimid, lat_varid) )
        call check( nf90_def_var(ncid_out, LON_NAME, NF90_REAL, lon_dimid, lon_varid) )

        ! Assign units attributes to coordinate variables.
        call check( nf90_put_att(ncid_out, lat_varid, UNITS, LAT_UNITS) )
        call check( nf90_put_att(ncid_out, lon_varid, UNITS, LON_UNITS) )
        call check( nf90_put_att(ncid_out, t_varid, UNITS, TIME_UNITS) )

        ! The dimids array is used to pass the dimids of the dimensions of the netCDF variables.
        dimids = (/lon_dimid, lat_dimid, lvl_dimid, t_dimid /)

        ! Define the netCDF variables for the prognositc and auxiliary data.
        call check( nf90_def_var(ncid_out, DENSITY_NAME, NF90_REAL, dimids, density_varid) )
        call check( nf90_def_var(ncid_out, U_NAME, NF90_REAL, dimids, u_varid) )
        call check( nf90_def_var(ncid_out, V_NAME, NF90_REAL, dimids, v_varid) )
        call check( nf90_def_var(ncid_out, W_NAME, NF90_REAL, dimids, w_varid) )       
        call check( nf90_def_var(ncid_out, THETA_NAME, NF90_REAL, dimids, theta_varid) )
        call check( nf90_def_var(ncid_out, PRES_NAME, NF90_REAL, dimids, pres_varid) )
        call check( nf90_def_var(ncid_out, HEIGHT_NAME, NF90_REAL, dimids, height_varid) )
        
        ! Assign units attributes to the netCDF variables.
        call check( nf90_put_att(ncid_out, density_varid, UNITS, DENSITY_UNITS) )
        call check( nf90_put_att(ncid_out, u_varid, UNITS, U_UNITS) )
        call check( nf90_put_att(ncid_out, v_varid, UNITS, V_UNITS) )
        call check( nf90_put_att(ncid_out, w_varid, UNITS, W_UNITS) )
        call check( nf90_put_att(ncid_out, theta_varid, UNITS, THETA_UNITS) )
        call check( nf90_put_att(ncid_out, pres_varid, UNITS, PRES_UNITS) )
        call check( nf90_put_att(ncid_out, height_varid, UNITS, HEIGHT_UNITS) )
        
        ! End define mode.
        call check( nf90_enddef(ncid_out) )
        
        ! write coordinate data
        call check( nf90_put_var(ncid_out, t_varid, secondsSinceEpoch_list) )
        call check( nf90_put_var(ncid_out, lat_varid, lats) )
        call check( nf90_put_var(ncid_out, lon_varid, lons) )
        count = (/ NLONS, NLATS, NLVLS, 1/)
        
        contains
            subroutine check(status)
                integer, intent (in) :: status
            
                if(status /= nf90_noerr) then 
                    print *, trim(nf90_strerror(status))
                    stop "Stopped"
                end if
            end subroutine check 
    end subroutine create_nc

    subroutine write_data2nc(t_rec)
        integer, intent (in) :: t_rec
        integer :: NLVLS, NLATS, NLONS, xx, yy, zz
        real :: density_out(grid_para%x_nums, grid_para%y_nums, grid_para%z_nums)
        real :: u_out(grid_para%x_nums, grid_para%y_nums, grid_para%z_nums)
        real :: v_out(grid_para%x_nums, grid_para%y_nums, grid_para%z_nums)
        real :: w_out(grid_para%x_nums, grid_para%y_nums, grid_para%z_nums)
        real :: th_out(grid_para%x_nums, grid_para%y_nums, grid_para%z_nums)
        real :: pres_out(grid_para%x_nums, grid_para%y_nums, grid_para%z_nums)

        NLVLS = grid_para%z_nums
        NLATS = grid_para%y_nums
        NLONS = grid_para%x_nums
        
        do zz = 1, NLVLS
            do yy = 1, NLATS
                do xx = 1, NLONS
                    density_out(xx, yy, zz) = prognostic_data_new%density(zz,yy,xx)
                    u_out(xx, yy, zz) = prognostic_data_new%u(zz,yy,xx)
                    v_out(xx, yy, zz) = prognostic_data_new%v(zz,yy,xx)
                    w_out(xx, yy, zz) = prognostic_data_new%w(zz,yy,xx)
                    th_out(xx, yy, zz) = prognostic_data_new%theta(zz,yy,xx)
                end do
            end do
        end do

        start = (/ 1, 1, 1, t_rec /)
        write(*,*) "save t_rec_ ",t_rec, "data to nc succeed"
        call check( nf90_put_var(ncid_out, density_varid, density_out, start = start, count = count) )              
        call check( nf90_put_var(ncid_out, u_varid, u_out, start = start, count = count) )              
        call check( nf90_put_var(ncid_out, v_varid, v_out, start = start, count = count) )              
        call check( nf90_put_var(ncid_out, w_varid, w_out, start = start, count = count) )              
        call check( nf90_put_var(ncid_out, theta_varid, th_out, start = start, count = count) )              
        ! call check( nf90_put_var(ncid_out, pres_varid, aux_data%pressure, start = start, count = count) )
        ! call check( nf90_put_var(ncid_out, height_varid, aux_data%height, start = start, count = count) )
        
        contains
            subroutine check(status)
                integer, intent (in) :: status
            
                if(status /= nf90_noerr) then 
                    print *, trim(nf90_strerror(status))
                    stop "Stopped"
                end if
            end subroutine check 
    end subroutine write_data2nc

    subroutine close_nc()
        call check( nf90_close(ncid_out) )  
        
        contains
        subroutine check(status)
            integer, intent (in) :: status
        
            if(status /= nf90_noerr) then 
                print *, trim(nf90_strerror(status))
                stop "Stopped"
            end if
        end subroutine check 
    end subroutine close_nc

    subroutine get_initial_from_nc()
        character (len = *), parameter :: FILE_NAME = "../data/init_2016-06-01_01:00:00.nc"
        integer :: ncid_initial, density_varid, pres_varid, theta_varid, u_varid, v_varid, w_varid
        integer, parameter :: NDIMS = 3
        integer :: start(NDIMS), count(NDIMS)
        integer :: NLVLS, NLATS, NLONS
        integer :: xx, yy, zz

        real :: density_in(grid_para%x_nums, grid_para%y_nums, grid_para%z_nums)
        real :: u_in(grid_para%x_nums, grid_para%y_nums, grid_para%z_nums)
        real :: v_in(grid_para%x_nums, grid_para%y_nums, grid_para%z_nums)
        real :: w_in(grid_para%x_nums, grid_para%y_nums, grid_para%z_nums)
        real :: th_in(grid_para%x_nums, grid_para%y_nums, grid_para%z_nums)
        real :: pres_in(grid_para%x_nums, grid_para%y_nums, grid_para%z_nums)

        NLVLS = grid_para%z_nums
        NLATS = grid_para%y_nums
        NLONS = grid_para%x_nums
        
        call check( nf90_open(FILE_NAME, NF90_NOWRITE, ncid_initial) )

        call check( nf90_inq_varid(ncid_initial, DENSITY_NAME, density_varid) )
        call check( nf90_inq_varid(ncid_initial, U_NAME, u_varid) )
        call check( nf90_inq_varid(ncid_initial, V_NAME, v_varid) )
        call check( nf90_inq_varid(ncid_initial, W_NAME, w_varid) )
        call check( nf90_inq_varid(ncid_initial, THETA_NAME, theta_varid) )        
        call check( nf90_inq_varid(ncid_initial, PRES_NAME, pres_varid) )

        count = (/ NLONS, NLATS, NLVLS/)

        start = (/ 1, 1, 1 /)

        call check( nf90_get_var(ncid_initial, density_varid, density_in, start = start, count = count) )
        call check( nf90_get_var(ncid_initial, u_varid, u_in, start = start, count = count) )
        call check( nf90_get_var(ncid_initial, v_varid, v_in, start = start, count = count) )
        call check( nf90_get_var(ncid_initial, w_varid, w_in, start = start, count = count) )
        call check( nf90_get_var(ncid_initial, theta_varid, th_in, start = start, count = count) )
        call check( nf90_get_var(ncid_initial, pres_varid, pres_in, start = start, count = count) )

        call check( nf90_close(ncid_initial) )

        do zz = 1, NLVLS
            do yy = 1, NLATS
                do xx = 1, NLONS
                    ini_data%density(zz,yy,xx) = density_in(xx, yy, zz)
                    ini_data%u(zz,yy,xx) = u_in(xx, yy, zz)
                    ini_data%v(zz,yy,xx) = v_in(xx, yy, zz)
                    ini_data%w(zz,yy,xx) = w_in(xx, yy, zz)
                    ini_data%theta(zz,yy,xx) = th_in(xx, yy, zz)
                    ini_data%pressure(zz,yy,xx) = pres_in(xx, yy, zz)
                end do
            end do
        end do

        write(*,*) "|>init data read succeed"
        write(*,*) "print first column" 
        write(*,*) "height(m),   pressure(hPa),   density(kgm-3),   u(ms-1),   v(ms-1),   w(ms-1),   theta,"        
        yy = 1
        xx = 1
        do zz = 1, NLVLS
            write(*,*) grid_para%center_bottom_height+(zz-1)*grid_para%y_interval,&
            ini_data%pressure(zz,yy,xx),&
            ini_data%density(zz,yy,zz),&
            ini_data%u(zz,yy,xx),&
            ini_data%v(zz,yy,xx),&
            ini_data%w(zz,yy,xx),&
            ini_data%theta(zz,yy,xx)
        end do
        
        contains
        subroutine check(status)
            integer, intent (in) :: status
        
            if(status /= nf90_noerr) then 
                print *, trim(nf90_strerror(status))
                stop "Stopped"
            end if
        end subroutine check 
        
    end subroutine get_initial_from_nc

    subroutine open_force_nc()
        character (len = *), parameter :: FILE_NAME = "../data/force_2016-06-01_01:00:00.nc"
        allocate(force_dt(time_para%run_hours * 3600 / time_para%t_interval))
        
        ! open nc
        call check( nf90_open(FILE_NAME, NF90_NOWRITE, ncid_force) )

        ! get var id
        call check( nf90_inq_varid(ncid_force, DENSITY_bottom_NAME, density_b_varid) )
        call check( nf90_inq_varid(ncid_force, DENSITY_top_NAME, density_t_varid) )
        call check( nf90_inq_varid(ncid_force, DENSITY_west_NAME, density_w_varid) )
        call check( nf90_inq_varid(ncid_force, DENSITY_east_NAME, density_e_varid) )
        call check( nf90_inq_varid(ncid_force, DENSITY_south_NAME, density_s_varid) )
        call check( nf90_inq_varid(ncid_force, DENSITY_north_NAME, density_n_varid) )
        
        call check( nf90_inq_varid(ncid_force, TH_bottom_NAME, theta_b_varid) )
        call check( nf90_inq_varid(ncid_force, TH_top_NAME,    theta_t_varid) )
        call check( nf90_inq_varid(ncid_force, TH_west_NAME,   theta_w_varid) )
        call check( nf90_inq_varid(ncid_force, TH_east_NAME,   theta_e_varid) )
        call check( nf90_inq_varid(ncid_force, TH_south_NAME,  theta_s_varid) )
        call check( nf90_inq_varid(ncid_force, TH_north_NAME,  theta_n_varid) )
        
        call check( nf90_inq_varid(ncid_force, U_bottom_NAME, u_b_varid) )
        call check( nf90_inq_varid(ncid_force, U_top_NAME,    u_t_varid) )
        call check( nf90_inq_varid(ncid_force, U_west_NAME,   u_w_varid) )
        call check( nf90_inq_varid(ncid_force, U_east_NAME,   u_e_varid) )
        call check( nf90_inq_varid(ncid_force, U_south_NAME,  u_s_varid) )
        call check( nf90_inq_varid(ncid_force, U_north_NAME,  u_n_varid) )
        
        call check( nf90_inq_varid(ncid_force, V_bottom_NAME, v_b_varid) )
        call check( nf90_inq_varid(ncid_force, V_top_NAME,    v_t_varid) )
        call check( nf90_inq_varid(ncid_force, V_west_NAME,   v_w_varid) )
        call check( nf90_inq_varid(ncid_force, V_east_NAME,   v_e_varid) )
        call check( nf90_inq_varid(ncid_force, V_south_NAME,  v_s_varid) )
        call check( nf90_inq_varid(ncid_force, V_north_NAME,  v_n_varid) )

        call check( nf90_inq_varid(ncid_force, W_bottom_NAME, w_b_varid) )
        call check( nf90_inq_varid(ncid_force, W_top_NAME,    w_t_varid) )
        call check( nf90_inq_varid(ncid_force, W_west_NAME,   w_w_varid) )
        call check( nf90_inq_varid(ncid_force, W_east_NAME,   w_e_varid) )
        call check( nf90_inq_varid(ncid_force, W_south_NAME,  w_s_varid) )
        call check( nf90_inq_varid(ncid_force, W_north_NAME,  w_n_varid) )
        
        call check( nf90_inq_varid(ncid_force, P_bottom_NAME, pres_b_varid) )
        call check( nf90_inq_varid(ncid_force, P_top_NAME,    pres_t_varid) )
        call check( nf90_inq_varid(ncid_force, P_west_NAME,   pres_w_varid) )
        call check( nf90_inq_varid(ncid_force, P_east_NAME,   pres_e_varid) )
        call check( nf90_inq_varid(ncid_force, P_south_NAME,  pres_s_varid) )
        call check( nf90_inq_varid(ncid_force, P_north_NAME,  pres_n_varid) )

        contains
        subroutine check(status)
            integer, intent (in) :: status
        
            if(status /= nf90_noerr) then 
                print *, trim(nf90_strerror(status))
                stop "Stopped"
            end if
        end subroutine check 
    end subroutine open_force_nc

    subroutine get_force_from_nc_at_time(t_rec)
        integer, intent(in) :: t_rec
        integer :: NLVLS, NLATS, NLONS
        integer, parameter :: NDIMS = 3
        integer :: start(NDIMS), count(NDIMS)
        integer :: xx, yy, zz

        ! bottom boundary
        real :: density_b_in(grid_para%x_nums, grid_para%y_nums)
        real ::       u_b_in(grid_para%x_nums, grid_para%y_nums)
        real ::       v_b_in(grid_para%x_nums, grid_para%y_nums)
        real ::       w_b_in(grid_para%x_nums, grid_para%y_nums)
        real ::      th_b_in(grid_para%x_nums, grid_para%y_nums)
        real ::    pres_b_in(grid_para%x_nums, grid_para%y_nums)
        ! top boundary
        real :: density_t_in(grid_para%x_nums, grid_para%y_nums)
        real ::       u_t_in(grid_para%x_nums, grid_para%y_nums)
        real ::       v_t_in(grid_para%x_nums, grid_para%y_nums)
        real ::       w_t_in(grid_para%x_nums, grid_para%y_nums)
        real ::      th_t_in(grid_para%x_nums, grid_para%y_nums)
        real ::    pres_t_in(grid_para%x_nums, grid_para%y_nums)
        ! west boundary
        real :: density_w_in(grid_para%y_nums, grid_para%z_nums)
        real ::       u_w_in(grid_para%y_nums, grid_para%z_nums)
        real ::       v_w_in(grid_para%y_nums, grid_para%z_nums)
        real ::       w_w_in(grid_para%y_nums, grid_para%z_nums)
        real ::      th_w_in(grid_para%y_nums, grid_para%z_nums)
        real ::    pres_w_in(grid_para%y_nums, grid_para%z_nums)
        ! west boundary
        real :: density_e_in(grid_para%y_nums, grid_para%z_nums)
        real ::       u_e_in(grid_para%y_nums, grid_para%z_nums)
        real ::       v_e_in(grid_para%y_nums, grid_para%z_nums)
        real ::       w_e_in(grid_para%y_nums, grid_para%z_nums)
        real ::      th_e_in(grid_para%y_nums, grid_para%z_nums)
        real ::    pres_e_in(grid_para%y_nums, grid_para%z_nums)
        ! south boundary
        real :: density_s_in(grid_para%x_nums, grid_para%z_nums)
        real ::       u_s_in(grid_para%x_nums, grid_para%z_nums)
        real ::       v_s_in(grid_para%x_nums, grid_para%z_nums)
        real ::       w_s_in(grid_para%x_nums, grid_para%z_nums)
        real ::      th_s_in(grid_para%x_nums, grid_para%z_nums)
        real ::    pres_s_in(grid_para%x_nums, grid_para%z_nums)
        ! north boundary
        real :: density_n_in(grid_para%x_nums, grid_para%z_nums)
        real ::       u_n_in(grid_para%x_nums, grid_para%z_nums)
        real ::       v_n_in(grid_para%x_nums, grid_para%z_nums)
        real ::       w_n_in(grid_para%x_nums, grid_para%z_nums)
        real ::      th_n_in(grid_para%x_nums, grid_para%z_nums)
        real ::    pres_n_in(grid_para%x_nums, grid_para%z_nums)
        
        NLVLS = grid_para%z_nums
        NLATS = grid_para%y_nums
        NLONS = grid_para%x_nums
        
        ! read bottom and top
        count = (/ NLONS, NLATS, 1/)
        start = (/ 1, 1, t_rec /)

        call check( nf90_get_var(ncid_force, density_b_varid, density_b_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       u_b_varid,       u_b_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       v_b_varid,       v_b_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       w_b_varid,       w_b_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,   theta_b_varid,      th_b_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,    pres_b_varid,    pres_b_in, start = start, count = count) )
        
        call check( nf90_get_var(ncid_force, density_t_varid, density_t_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       u_t_varid,       u_t_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       v_t_varid,       v_t_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       w_t_varid,       w_t_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,   theta_t_varid,      th_t_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,    pres_t_varid,    pres_t_in, start = start, count = count) )
    
        ! read west and east
        count = (/ NLATS, NLVLS, 1/)
        start = (/ 1, 1, t_rec /)

        call check( nf90_get_var(ncid_force, density_e_varid, density_e_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       u_e_varid,       u_e_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       v_e_varid,       v_e_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       w_e_varid,       w_e_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,   theta_e_varid,      th_e_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,    pres_e_varid,    pres_e_in, start = start, count = count) )
        
        call check( nf90_get_var(ncid_force, density_w_varid, density_w_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       u_w_varid,       u_w_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       v_w_varid,       v_w_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       w_w_varid,       w_w_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,   theta_w_varid,      th_w_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,    pres_w_varid,    pres_w_in, start = start, count = count) )

        ! read south and north
        count = (/ NLONS, NLVLS, 1/)
        start = (/ 1, 1, t_rec /)

        call check( nf90_get_var(ncid_force, density_s_varid, density_s_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       u_s_varid,       u_s_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       v_s_varid,       v_s_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       w_s_varid,       w_s_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,   theta_s_varid,      th_s_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,    pres_s_varid,    pres_s_in, start = start, count = count) )
        
        call check( nf90_get_var(ncid_force, density_n_varid, density_n_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       u_n_varid,       u_n_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       v_n_varid,       v_n_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,       w_n_varid,       w_n_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,   theta_n_varid,      th_n_in, start = start, count = count) )
        call check( nf90_get_var(ncid_force,    pres_n_varid,    pres_n_in, start = start, count = count) )

        ! pass data to force type variable
        do yy = 1, NLATS
            do xx = 1, NLONS
                force_data_bottom%density(yy,xx)  = density_b_in(xx,yy)
                force_data_bottom%pressure(yy,xx) =    pres_b_in(xx,yy)
                force_data_bottom%u(yy,xx)        =       u_b_in(xx,yy)
                force_data_bottom%v(yy,xx)        =       v_b_in(xx,yy)
                force_data_bottom%w(yy,xx)        =       w_b_in(xx,yy)
                force_data_bottom%theta(yy,xx)    =      th_b_in(xx,yy)

                force_data_top%density(yy,xx)  = density_t_in(xx,yy)
                force_data_top%pressure(yy,xx) =    pres_t_in(xx,yy)
                force_data_top%u(yy,xx)        =       u_t_in(xx,yy)
                force_data_top%v(yy,xx)        =       v_t_in(xx,yy)
                force_data_top%w(yy,xx)        =       w_t_in(xx,yy)
                force_data_top%theta(yy,xx)    =      th_t_in(xx,yy)
            end do
        end do 

        do yy = 1, NLVLS
            do xx = 1, NLATS
                force_data_west%density(yy,xx)  = density_w_in(xx,yy)
                force_data_west%pressure(yy,xx) =    pres_w_in(xx,yy)
                force_data_west%u(yy,xx)        =       u_w_in(xx,yy)
                force_data_west%v(yy,xx)        =       v_w_in(xx,yy)
                force_data_west%w(yy,xx)        =       w_w_in(xx,yy)
                force_data_west%theta(yy,xx)    =      th_w_in(xx,yy)

                force_data_east%density(yy,xx)  = density_e_in(xx,yy)
                force_data_east%pressure(yy,xx) =    pres_e_in(xx,yy)
                force_data_east%u(yy,xx)        =       u_e_in(xx,yy)
                force_data_east%v(yy,xx)        =       v_e_in(xx,yy)
                force_data_east%w(yy,xx)        =       w_e_in(xx,yy)
                force_data_east%theta(yy,xx)    =      th_e_in(xx,yy)
            end do
        end do

        do yy = 1, NLVLS
            do xx = 1, NLONS
                force_data_south%density(yy,xx)  = density_s_in(xx,yy)
                force_data_south%pressure(yy,xx) =    pres_s_in(xx,yy)
                force_data_south%u(yy,xx)        =       u_s_in(xx,yy)
                force_data_south%v(yy,xx)        =       v_s_in(xx,yy)
                force_data_south%w(yy,xx)        =       w_s_in(xx,yy)
                force_data_south%theta(yy,xx)    =      th_s_in(xx,yy)

                force_data_north%density(yy,xx)  = density_n_in(xx,yy)
                force_data_north%pressure(yy,xx) =    pres_n_in(xx,yy)
                force_data_north%u(yy,xx)        =       u_n_in(xx,yy)
                force_data_north%v(yy,xx)        =       v_n_in(xx,yy)
                force_data_north%w(yy,xx)        =       w_n_in(xx,yy)
                force_data_north%theta(yy,xx)    =      th_n_in(xx,yy)
            end do
        end do
        write(*,*) "|> force data read succeed at time index: ", t_rec

        contains
        subroutine check(status)
            integer, intent (in) :: status
        
            if(status /= nf90_noerr) then 
                print *, trim(nf90_strerror(status))
                stop "Stopped"
            end if
        end subroutine check
    end subroutine get_force_from_nc_at_time

    subroutine close_force_nc()    
        call check( nf90_close(ncid_force) )  
        
        contains
        subroutine check(status)
            integer, intent (in) :: status
        
            if(status /= nf90_noerr) then 
                print *, trim(nf90_strerror(status))
                stop "Stopped"
            end if
        end subroutine check
    end subroutine close_force_nc

end module IO