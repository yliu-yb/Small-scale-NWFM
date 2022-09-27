!> Small-scale Numerical Weather Forecast Model (SsNWFM)
! hello
program SsNWFM
    use data, only : read_time_grid_config, initial_data_set
    use dynamicSolver, only : IO_test
    implicit none

    write(*,*) '|> read time and grid config form ../data/time_grid.config'
    call read_time_grid_config()

    ! allocate memory and initial data value to 0
    call initial_data_set()

    call IO_test()

    ! write(*,*) '|> read initial field form ../data/initial_test.nc'
    ! call read_initial_data()

    ! write(*,*) '|> read force field form ../data/force_test.nc'
    ! call read_force_data()
    
    ! write(*,*) SHAPE(ini_data%density)
    ! write(*,*) SHAPE(force_data_bottom%pressure)
    ! write(*,*) SHAPE(force_data_west%pressure)

end program SsNWFM