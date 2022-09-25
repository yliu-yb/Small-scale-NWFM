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

    contains
    subroutine read_time_grid_config()
        open(1, file = '../data/time_grid.config', status = 'old')
        
        read(1,*) time_para%start_date_time
        read(1,*) time_para%end_date_time
        read(1,*) time_para%run_hours
        read(1,*) time_para%t_interval
        read(1,*) grid_para%center_lon
        read(1,*) grid_para%center_lat
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
        write(*,*) 'integral t(s):',time_para%t_interval
        write(*,*) 'center_lon:',grid_para%center_lon
        write(*,*) 'center_lat:',grid_para%center_lat
        write(*,*) 'x-direction gird num:',grid_para%x_nums
        write(*,*) 'y-direction gird num',grid_para%y_nums
        write(*,*) 'z-direction gird num',grid_para%z_nums
        write(*,*) 'x interval distance(m):',grid_para%x_interval
        write(*,*) 'y interval distance(m):',grid_para%y_interval
        write(*,*) 'z interval distance(m):',grid_para%z_interval
    end subroutine read_time_grid_config
    
    subroutine read_initial_data()
        integer(KIND=4) :: ncid
        character (len = *), parameter :: FILE_NAME = "../data/simple_xy.nc"

        allocate (ini_data%density(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (ini_data%theta(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (ini_data%u(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (ini_data%v(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (ini_data%w(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (ini_data%q(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))

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
        integer :: t_nums
        integer :: retval                

        t_nums = time_para%run_hours * 3600 / time_para%t_interval
        
        retval = allocate_force_data(force_data_bottom, t_nums, grid_para%y_nums, grid_para%x_nums)
        retval = allocate_force_data(force_data_top, t_nums, grid_para%y_nums, grid_para%x_nums)
        retval = allocate_force_data(force_data_west, t_nums, grid_para%z_nums, grid_para%y_nums)
        retval = allocate_force_data(force_data_east, t_nums, grid_para%z_nums, grid_para%y_nums)
        retval = allocate_force_data(force_data_south, t_nums, grid_para%z_nums, grid_para%x_nums)
        retval = allocate_force_data(force_data_north, t_nums, grid_para%z_nums, grid_para%x_nums)

        contains
            function allocate_force_data(f_data, index_first, index_second, index_third) result(retval)
                type(force_data) :: f_data
                integer :: index_first
                integer :: index_second
                integer :: index_third
                integer :: retval                

                allocate(f_data%pressure(index_first, index_second, index_third))
                allocate(f_data%temprature(index_first, index_second, index_third))
                allocate(f_data%u(index_first, index_second, index_third))
                allocate(f_data%v(index_first, index_second, index_third))
                allocate(f_data%w(index_first, index_second, index_third))
                allocate(f_data%q(index_first, index_second, index_third))

            end function allocate_force_data

    end subroutine read_force_data

end module data