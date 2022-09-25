!> Small-scale Numerical Weather Forecast Model (SsNWFM)
! hello
program SsNWFM
    use data
    implicit none

    write(*,*) '|> read time_grid config file form ../data/time_grid.config'
    call read_time_grid_config()

    write(*,*) '|> read initial file form ../data/initial_test.nc'
    call read_initial_data

    write(*,*) '|> read force file form ../data/force_test.nc'
    call read_force_data
    
    write(*,*) SHAPE(ini_data%density)
    write(*,*) SHAPE(force_data_bottom%pressure)
    write(*,*) SHAPE(force_data_west%pressure)

end program SsNWFM