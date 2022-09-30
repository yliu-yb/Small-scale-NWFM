!------------------------------------------------------------------------------
! Hello Numerical Weather Forecast!
!------------------------------------------------------------------------------
!
! MODULE: data
!
!> @author
!> {yan liu}
!
! DESCRIPTION: 
!>  global data used to run model.
!>  data read form config file, initial file, force file.
!>  Note: the gird dimension of initial and force file data must meet the config grid nums, otherwise, data module may crashed.
!>  Note: the time dimension of force file data must meet the config run_hours*3600/integral_t, otherwise, data module may crashed.
!
! REVISION HISTORY:
! 25 09 2022 - Initial Version:
! defined global variables.
! Added read_time_grid_config() subroutine, 
! Build the framework of the read_initial_data() and read_force_data() subrountines.
! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
!------------------------------------------------------------------------------

module data
    use netcdf
    use customTypes

    implicit none

    type(time_parameter) :: time_para
    type(grid_parameter) :: grid_para

    type(initial_data) :: ini_data
    type(force_data) :: force_data_bottom
    type(force_data) :: force_data_top
    type(force_data) :: force_data_west
    type(force_data) :: force_data_east
    type(force_data) :: force_data_south
    type(force_data) :: force_data_north

    type(prognostic_data) :: prognostic_data_new
    type(prognostic_data) :: prognostic_data_old

    type(auxiliary_data) :: aux_data

    integer(KIND = 8), dimension(:), allocatable :: secondsSinceEpoch_list
    real, dimension(:), allocatable :: lons
    real, dimension(:), allocatable :: lats

    real, parameter :: coe_lat2dis = 110574
    real, parameter :: coe_lon2dis = 111320
    real, parameter :: coe_radian = 3.14 / 180

    contains

    subroutine initial_data_set()
        integer :: t_nums
        integer :: retval

        ! initial data
        allocate (ini_data%density(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (ini_data%theta(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (ini_data%u(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (ini_data%v(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (ini_data%w(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (ini_data%q(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (ini_data%pressure(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))

        ! auxiliary data
        allocate (aux_data%pressure(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (aux_data%height(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))

        ! force data
        t_nums = time_para%run_hours * 3600 / time_para%t_interval
        
        retval = allocate_force_data(force_data_bottom, grid_para%y_nums, grid_para%x_nums)
        retval = allocate_force_data(force_data_top,    grid_para%y_nums, grid_para%x_nums)
        retval = allocate_force_data(force_data_west,   grid_para%z_nums, grid_para%y_nums)
        retval = allocate_force_data(force_data_east,   grid_para%z_nums, grid_para%y_nums)
        retval = allocate_force_data(force_data_south,  grid_para%z_nums, grid_para%x_nums)
        retval = allocate_force_data(force_data_north,  grid_para%z_nums, grid_para%x_nums)

        ! prognositic data
        allocate (prognostic_data_new%density(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (prognostic_data_new%theta(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (prognostic_data_new%u(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (prognostic_data_new%v(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (prognostic_data_new%w(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (prognostic_data_new%q(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))

        allocate (prognostic_data_old%density(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (prognostic_data_old%theta(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (prognostic_data_old%u(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (prognostic_data_old%v(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (prognostic_data_old%w(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (prognostic_data_old%q(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        
        contains
            function allocate_force_data(f_data, index_first, index_second) result(retval)
                type(force_data) :: f_data
                integer :: index_first
                integer :: index_second
                integer :: index_third
                integer :: retval                

                allocate(f_data%density(index_first, index_second))
                allocate(f_data%pressure(index_first, index_second))
                allocate(f_data%theta(index_first, index_second))
                allocate(f_data%u(index_first, index_second))
                allocate(f_data%v(index_first, index_second))
                allocate(f_data%w(index_first, index_second))
                allocate(f_data%q(index_first, index_second))
                
                retval = 1
            end function allocate_force_data

    end subroutine initial_data_set

    subroutine read_time_grid_config()
        use datetime_module, only : datetime, timedelta, tm_struct, c_strptime, tm2date
        type(datetime)  :: dt
        type(timedelta) :: timediff
        type(tm_struct) :: ctime
        integer         :: rc, i, t_nums

        open(1, file = '../data/time_grid.config', status = 'old')
        
        read(1,*) time_para%start_date_time
        read(1,*) time_para%end_date_time
        read(1,*) time_para%run_hours
        read(1,*) time_para%t_interval
        read(1,*) grid_para%center_lon
        read(1,*) grid_para%center_lat
        read(1,*) grid_para%center_bottom_height
        read(1,*) grid_para%x_nums
        read(1,*) grid_para%y_nums
        read(1,*) grid_para%z_nums
        read(1,*) grid_para%x_interval
        read(1,*) grid_para%y_interval
        read(1,*) grid_para%z_interval
        
        close(1)
        
        write(*,*) '*******config info*******'
        write(*,*) 'start_datetime:',time_para%start_date_time
        write(*,*) 'end_datetime:',time_para%end_date_time
        write(*,*) 'run_hours(h):',time_para%run_hours
        write(*,*) 'integral_t(s):',time_para%t_interval
        write(*,*) 'center_lon:',grid_para%center_lon
        write(*,*) 'center_lat:',grid_para%center_lat
        write(*,*) 'center_bottom_height:',grid_para%center_bottom_height
        write(*,*) 'x-direction_gird_num:',grid_para%x_nums
        write(*,*) 'y-direction_gird_num:',grid_para%y_nums
        write(*,*) 'z-direction_gird_num:',grid_para%z_nums
        write(*,*) 'x_interval_distance(m):',grid_para%x_interval
        write(*,*) 'y_interval_distance(m):',grid_para%y_interval
        write(*,*) 'z_interval_distance(m):',grid_para%z_interval

        ! make integral datetime list 
        t_nums = time_para%run_hours * 3600 / time_para%t_interval
        allocate(secondsSinceEpoch_list(t_nums))

        rc = c_strptime(time_para%start_date_time,"%Y-%m-%d_%H:%M:%S", ctime)
        dt = tm2date(ctime)
        
        do i = 1, t_nums
            timediff = timedelta(seconds = time_para%t_interval)
            dt = dt + timediff
            secondsSinceEpoch_list(i) = dt%secondsSinceEpoch()
        end do 

        ! make lon lats lists
        allocate(lons(grid_para%x_nums))
        allocate(lats(grid_para%y_nums))

        do i = 1, grid_para%y_nums
            lats(i) = grid_para%center_lat + (i - grid_para%y_nums / 2) * grid_para%y_interval / coe_lat2dis
        end do
        
        do i = 1, grid_para%x_nums
            lons(i) = grid_para%center_lon + (i - grid_para%x_nums / 2) * grid_para%x_interval &
            / (coe_lat2dis * COS(grid_para%center_lat * coe_radian))
        end do
        
    end subroutine read_time_grid_config
    
    subroutine read_initial_data()
        integer(KIND=4) :: ncid
        character (len = *), parameter :: FILE_NAME = "../data/simple_xy.nc"

        ! netcdf test
        write (*,*) 'ncid'
        call check( nf90_open(FILE_NAME, NF90_NOWRITE, ncid) )
        call check( nf90_close(ncid) )
        write (*,*) ncid
        contains
            subroutine check(status)
                integer, intent ( in) :: status
                
                if(status /= nf90_noerr) then 
                print *, trim(nf90_strerror(status))
                stop "Stopped"
                end if
            end subroutine check
    end subroutine read_initial_data

    subroutine read_force_data()
        
    end subroutine read_force_data

end module data