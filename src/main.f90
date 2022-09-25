!> Small-scale Numerical Weather Forcast Model (SsNWFM)
! hello
program SsNWFM
    use data
    implicit none

    write(*,*) 'read time_grid config file form ../data/time_grid.config'
    call read_time_grid_config()
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
    

end program SsNWFM