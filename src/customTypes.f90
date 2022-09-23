module customTypes
    implicit none
    
    type, public :: time_para
        character(len = 19) :: start_date_time ! yyyy-mm-dd_hh:mm:ss
        character(len = 19) :: end_date_time
        integer :: run_hours
        integer :: t_interval
    end type time_para

    type, public :: grid_para
        real :: center_lon
        real :: center_lat
        real :: center_bottom_height
        real :: x_nums
        real :: y_nums
        real :: z_nums
        real :: x_interval
        real :: y_interval
        real :: z_interval
    end type grid_para

    type, public :: initial_data
        real, dimension(:, :), allocatable :: bottom_pressure ! dimension(x,y)
        real, dimension(:, :, :), allocatable :: temprature ! dimension(x,y,z)
        real, dimension(:, :, :), allocatable :: u ! dimension(x,y,z)
        real, dimension(:, :, :), allocatable :: v ! dimension(x,y,z)
        real, dimension(:, :, :), allocatable :: w ! dimension(x,y,z)
        real, dimension(:, :, :), allocatable :: q ! dimension(x,y,z)
    end type initial_data
    
    type, public :: force_data
        real, dimension(:, :, :), allocatable :: pressure ! dimension(t,east/west/south/north/up/bottom)
        real, dimension(:, :, :), allocatable :: temprature ! dimension(t,east/west/south/north/up/bottom)
        real, dimension(:, :, :), allocatable :: u ! dimension(t,east/west/south/north/up/bottom)
        real, dimension(:, :, :), allocatable :: v ! dimension(t,east/west/south/north/up/bottom)
        real, dimension(:, :, :), allocatable :: w ! dimension(t,east/west/south/north/up/bottom)
        real, dimension(:, :, :), allocatable :: q ! dimension(t,east/west/south/north/up/bottom)
        end type force_data

    type, public :: prognostic_data
        real, dimension(:, :, :), allocatable :: density
        real, dimension(:, :, :), allocatable :: theta
        real, dimension(:, :, :), allocatable :: u
        real, dimension(:, :, :), allocatable :: v
        real, dimension(:, :, :), allocatable :: w
        real, dimension(:, :, :), allocatable :: q
    end type prognostic_data

contains
    
end module customTypes