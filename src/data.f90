!------------------------------------------------------------------------------
! Hello Numerical Weather Forcast!
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
    end subroutine read_time_grid_config
    
    subroutine read_initial_data()
        allocate (ini_data%density(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (ini_data%theta(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (ini_data%u(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (ini_data%v(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (ini_data%w(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        allocate (ini_data%q(grid_para%z_nums, grid_para%y_nums, grid_para%x_nums))
        
    end subroutine read_initial_data

end module data