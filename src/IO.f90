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
    integer :: ncid
    integer :: lvl_dimid, lon_dimid, lat_dimid, t_dimid
    integer :: lon_varid, lat_varid, t_varid
    integer :: density_varid, u_varid, v_varid, w_varid, theta_varid, pres_varid, height_varid

    integer, parameter :: NDIMS = 4
    integer :: dimids(NDIMS)

    character (len = *), parameter :: TIME_NAME = "times"
    character (len = *), parameter :: LVL_NAME = "levels"
    character (len = *), parameter :: LAT_NAME = "latitudes"
    character (len = *), parameter :: LON_NAME = "longitudes"

    character (len = *), parameter :: DENSITY_NAME = "density"
    character (len = *), parameter :: U_NAME = "x-direction velocity"
    character (len = *), parameter :: V_NAME = "y-direction velocity"
    character (len = *), parameter :: W_NAME = "vertical velocity"
    character (len = *), parameter :: THETA_NAME = "potential temperature"
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
        
        call check( nf90_create(path=FILE_NAME, cmode=or(nf90_clobber,nf90_64bit_offset), ncid=ncid) )
        ! define the dimensions
        call check( nf90_def_dim(ncid, TIME_NAME, NTIMES, t_dimid) )
        call check( nf90_def_dim(ncid, LVL_NAME, NLVLS, lvl_dimid) )
        call check( nf90_def_dim(ncid, LAT_NAME, NLATS, lat_dimid) )
        call check( nf90_def_dim(ncid, LON_NAME, NLONS, lon_dimid) )
        
        ! define coordinate variables
        call check( nf90_def_var(ncid, TIME_NAME, NF90_INT, t_dimid, t_varid) )
        call check( nf90_def_var(ncid, LAT_NAME, NF90_REAL, lat_dimid, lat_varid) )
        call check( nf90_def_var(ncid, LON_NAME, NF90_REAL, lon_dimid, lon_varid) )

        ! Assign units attributes to coordinate variables.
        call check( nf90_put_att(ncid, lat_varid, UNITS, LAT_UNITS) )
        call check( nf90_put_att(ncid, lon_varid, UNITS, LON_UNITS) )
        call check( nf90_put_att(ncid, t_varid, UNITS, TIME_UNITS) )

        ! The dimids array is used to pass the dimids of the dimensions of the netCDF variables.
        dimids = (/ t_dimid, lvl_dimid, lat_dimid, lon_dimid /)

        ! Define the netCDF variables for the prognositc and auxiliary data.
        call check( nf90_def_var(ncid, DENSITY_NAME, NF90_REAL, dimids, density_varid) )
        call check( nf90_def_var(ncid, U_NAME, NF90_REAL, dimids, u_varid) )
        call check( nf90_def_var(ncid, V_NAME, NF90_REAL, dimids, v_varid) )
        call check( nf90_def_var(ncid, W_NAME, NF90_REAL, dimids, w_varid) )       
        call check( nf90_def_var(ncid, THETA_NAME, NF90_REAL, dimids, theta_varid) )
        call check( nf90_def_var(ncid, PRES_NAME, NF90_REAL, dimids, pres_varid) )
        call check( nf90_def_var(ncid, HEIGHT_NAME, NF90_REAL, dimids, height_varid) )
        
        ! Assign units attributes to the netCDF variables.
        call check( nf90_put_att(ncid, density_varid, UNITS, DENSITY_UNITS) )
        call check( nf90_put_att(ncid, u_varid, UNITS, U_UNITS) )
        call check( nf90_put_att(ncid, v_varid, UNITS, V_UNITS) )
        call check( nf90_put_att(ncid, w_varid, UNITS, W_UNITS) )
        call check( nf90_put_att(ncid, theta_varid, UNITS, THETA_UNITS) )
        call check( nf90_put_att(ncid, pres_varid, UNITS, PRES_UNITS) )
        call check( nf90_put_att(ncid, height_varid, UNITS, HEIGHT_UNITS) )
        
        ! End define mode.
        call check( nf90_enddef(ncid) )
        
        ! write coordinate data
        call check( nf90_put_var(ncid, t_varid, secondsSinceEpoch_list) )
        call check( nf90_put_var(ncid, lat_varid, lats) )
        call check( nf90_put_var(ncid, lon_varid, lons) )
        count = (/ 1, NLVLS, NLATS, NLONS/)
        
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
        start = (/ t_rec, 1, 1, 1 /)
        write(*,*) t_rec
        call check( nf90_put_var(ncid, theta_varid, prognostic_data_new%density, start = start, count = count) )              
        call check( nf90_put_var(ncid, theta_varid, prognostic_data_new%u, start = start, count = count) )              
        call check( nf90_put_var(ncid, theta_varid, prognostic_data_new%v, start = start, count = count) )              
        call check( nf90_put_var(ncid, theta_varid, prognostic_data_new%w, start = start, count = count) )              
        call check( nf90_put_var(ncid, theta_varid, prognostic_data_new%theta, start = start, count = count) )              
        call check( nf90_put_var(ncid, pres_varid, aux_data%pressure, start = start, count = count) )
        call check( nf90_put_var(ncid, pres_varid, aux_data%height, start = start, count = count) )
        
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
        call check( nf90_close(ncid) )  
        
        contains
        subroutine check(status)
            integer, intent (in) :: status
        
            if(status /= nf90_noerr) then 
                print *, trim(nf90_strerror(status))
                stop "Stopped"
            end if
        end subroutine check 
    end subroutine close_nc
end module IO