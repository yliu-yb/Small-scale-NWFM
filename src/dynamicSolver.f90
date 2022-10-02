!------------------------------------------------------------------------------
! Hello Numerical Weather Forcast!
!------------------------------------------------------------------------------
!
! MODULE:  Dynamic Solver
!
!> @author
!> yan liu}
!
! DESCRIPTION: 
!> Small-scale Numerial Weather Forcast Model dynamic solver for temporal prediction and spatial discretisation 
!
! REVISION HISTORY:
! 25 09 2022 - Initial Version
! TODO_dd_mmm_yyyy - TODO_describe_appropriate_changes - TODO_name
!------------------------------------------------------------------------------

module dynamicSolver
    use data
    implicit none
    
contains
    subroutine IO_test()
        use IO
        integer         :: i, j, t_nums
        t_nums = time_para%run_hours * 3600 / time_para%t_interval

        ! test creat output nc
        ! call create_nc()
        
        ! do i = 1, t_nums
        !     prognostic_data_new%density = i
        !     prognostic_data_new%u = i
        !     prognostic_data_new%v = i
        !     prognostic_data_new%w = i
        !     prognostic_data_new%theta = i
        !     aux_data%pressure=i
        !     aux_data%height=i
        !     call write_data2nc(i)
        ! end do

        ! call close_nc() 
              
        ! test get force field data at every timestamp        
        call open_force_nc()
        
        do i = 1, t_nums
            call get_force_from_nc_at_time(i)
            write(*,*) "force_data_bottom"
            do j = 1, ubound(force_data_bottom%pressure, 1)
                print *, force_data_bottom%pressure(j, :)
            end do

            write(*,*) "force_data_top"
            do j = 1, ubound(force_data_top%pressure, 1)
                print *, force_data_top%pressure(j, :)
            end do
        end do

        call close_force_nc()
        
        ! test get initial field data from nc
        call get_initial_from_nc()

    end subroutine IO_test

    subroutine run()
        use IO
        integer :: t, t_end, t_interval, t_rec
        integer :: x, y, z, x_end, y_end, z_end, x_interval, y_interval, z_interval
        real :: coe_t_xdis, coe_t_ydis, coe_t_zdis
        real :: f, g
        real :: result
        real :: t1, t2, t3, t4, t5, t6, t7, t8, t9
        integer :: debug_mk  

        debug_mk = 0


        t_end = time_para%run_hours * 3600
        t_interval = time_para%t_interval
        x_interval = grid_para%x_interval
        y_interval = grid_para%y_interval
        z_interval = grid_para%z_interval
        
        x_end = grid_para%x_nums
        y_end = grid_para%y_nums
        z_end = grid_para%z_nums

        ! Geospin parameter
        f = 2 * 7.2921150 * 0.00001 * SIN(grid_para%center_lat * coe_radian)
        
        ! Gravitational acceleration  
        g = 9.8

        ! common coeficients
        coe_t_xdis = t_interval / float(x_interval)
        coe_t_ydis = t_interval / float(y_interval)
        coe_t_zdis = t_interval / float(z_interval)
        
        call create_nc()
        ! get initial field data and pass to prognostic_old 
        call get_initial_from_nc()
            
        prognostic_data_old%density(2:z_end + 1,2:y_end + 1,2:x_end + 1) = ini_data%density
        prognostic_data_old%u(2:z_end + 1,2:y_end + 1,2:x_end + 1) = ini_data%u
        prognostic_data_old%v(2:z_end + 1,2:y_end + 1,2:x_end + 1) = ini_data%v
        prognostic_data_old%w(2:z_end + 1,2:y_end + 1,2:x_end + 1) = ini_data%w
        prognostic_data_old%theta(2:z_end + 1,2:y_end + 1,2:x_end + 1) = ini_data%theta
        aux_data%pressure(2:z_end + 1,2:y_end + 1,2:x_end + 1) = ini_data%pressure
        
        call open_force_nc()
        t_rec = 0
        do t = 1, t_end, t_interval
            t_rec = t_rec + 1
            write(*,*) "process t_rec:", t_rec
            ! get force field data at time t_rec from nc file
            call get_force_from_nc_at_time(t_rec)
            
            ! bottom
            prognostic_data_old%density(1, 2:y_end+1, 2:x_end+1) = force_data_bottom%density
            prognostic_data_old%u(1, 2:y_end+1, 2:x_end+1) = force_data_bottom%u
            prognostic_data_old%v(1, 2:y_end+1, 2:x_end+1) = force_data_bottom%v
            prognostic_data_old%w(1, 2:y_end+1, 2:x_end+1) = force_data_bottom%w
            prognostic_data_old%theta(1, 2:y_end+1, 2:x_end+1) = force_data_bottom%theta
            aux_data%pressure(1, 2:y_end+1, 2:x_end+1) = force_data_bottom%pressure
            ! top
            prognostic_data_old%density(z_end + 2, 2:y_end+1, 2:x_end+1) = force_data_top%density
            prognostic_data_old%u(z_end + 2, 2:y_end+1, 2:x_end+1) = force_data_top%u
            prognostic_data_old%v(z_end + 2, 2:y_end+1, 2:x_end+1) = force_data_top%v
            prognostic_data_old%w(z_end + 2, 2:y_end+1, 2:x_end+1) = force_data_top%w
            prognostic_data_old%theta(z_end + 2, 2:y_end+1, 2:x_end+1) = force_data_top%theta
            aux_data%pressure(z_end + 2, 2:y_end+1, 2:x_end+1) = force_data_top%pressure
            ! west
            prognostic_data_old%density(2:z_end+1, 2:y_end+1, 1) = force_data_west%density
            prognostic_data_old%u(2:z_end+1, 2:y_end+1, 1) = force_data_west%u
            prognostic_data_old%v(2:z_end+1, 2:y_end+1, 1) = force_data_west%v
            prognostic_data_old%w(2:z_end+1, 2:y_end+1, 1) = force_data_west%w
            prognostic_data_old%theta(2:z_end+1, 2:y_end+1, 1) = force_data_west%theta
            aux_data%pressure(2:z_end+1, 2:y_end+1, 1) = force_data_west%pressure
            ! east
            prognostic_data_old%density(2:z_end+1, 2:y_end+1,  x_end + 2) = force_data_east%density
            prognostic_data_old%u(2:z_end+1, 2:y_end+1,  x_end + 2) = force_data_east%u
            prognostic_data_old%v(2:z_end+1, 2:y_end+1,  x_end + 2) = force_data_east%v
            prognostic_data_old%w(2:z_end+1, 2:y_end+1,  x_end + 2) = force_data_east%w
            prognostic_data_old%theta(2:z_end+1, 2:y_end+1,  x_end + 2) = force_data_east%theta
            aux_data%pressure(2:z_end+1, 2:y_end+1,  x_end + 2) = force_data_east%pressure
            ! south
            prognostic_data_old%density(2:z_end+1, 1, 2:x_end+1) = force_data_south%density
            prognostic_data_old%u(2:z_end+1, 1, 2:x_end+1) = force_data_south%u
            prognostic_data_old%v(2:z_end+1, 1, 2:x_end+1) = force_data_south%v
            prognostic_data_old%w(2:z_end+1, 1, 2:x_end+1) = force_data_south%w
            prognostic_data_old%theta(2:z_end+1, 1, 2:x_end+1) = force_data_south%theta
            aux_data%pressure(2:z_end+1, 1, 2:x_end+1) = force_data_south%pressure
            ! north
            prognostic_data_old%density(2:z_end+1, y_end + 2, 2:x_end+1) = force_data_north%density
            prognostic_data_old%u(2:z_end+1, y_end + 2, 2:x_end+1) = force_data_north%u
            prognostic_data_old%v(2:z_end+1, y_end + 2, 2:x_end+1) = force_data_north%v
            prognostic_data_old%w(2:z_end+1, y_end + 2, 2:x_end+1) = force_data_north%w
            prognostic_data_old%theta(2:z_end+1, y_end + 2, 2:x_end+1) = force_data_north%theta
            aux_data%pressure(2:z_end+1, y_end + 2, 2:x_end+1) = force_data_north%pressure
            write(*,*) "bottom boundary w", prognostic_data_old%w(2, 2, 1) 
            do z = 1, z_end
                do y = 1, y_end 
                    do x = 1, x_end
                        ! density
                        prognostic_data_new%density(z, y, x) = prognostic_data_old%density(z + 1, y  + 1, x + 1) &
                        - coe_t_xdis * (linear_interp(0, x, y, z, 1, 0, 0) * linear_interp(1, x, y, z, 1, 0, 0) &
                        - linear_interp(0, x, y, z, -1, 0, 0) * linear_interp(1, x, y, z, -1, 0, 0))&
                        - coe_t_ydis * (linear_interp(0, x, y, z, 0, 1, 0) * linear_interp(2, x, y, z, 0, 1, 0) &
                        - linear_interp(0, x, y, z, 0, -1, 0) * linear_interp(2, x, y, z, 0, -1, 0))&
                        - coe_t_zdis * (linear_interp(0, x, y, z, 0, 0, 1) * linear_interp(3, x, y, z, 0, 0, 1) &
                        - linear_interp(0, x, y, z, 0, 0, -1) * linear_interp(3, x, y, z, 0, 0, -1))
                        
                        if (ABS(prognostic_data_new%density(z, y, x) - prognostic_data_old%density(z + 1, y  + 1, x + 1)) > 0.2&
                         .and. debug_mk == 1) then
                            write(*,*) "z,y,x",z,y,x

                            write(*,*) "prognostic_data_new%density(z, y, x)", prognostic_data_new%density(z, y, x)
                            write(*,*) "prognostic_data_old%density(z + 1, y + 1, x + 1):", &
                            prognostic_data_old%density(z + 1, y + 1, x + 1)

                            t3 = coe_t_xdis * (linear_interp(0, x, y, z, 1, 0, 0) * linear_interp(1, x, y, z, 1, 0, 0)&
                            - linear_interp(0, x, y, z, -1, 0, 0) * linear_interp(1, x, y, z, -1, 0, 0))
                            t4 = coe_t_ydis * (linear_interp(0, x, y, z, 0, 1, 0) * linear_interp(2, x, y, z, 0, 1, 0)&
                            - linear_interp(0, x, y, z, 0, -1, 0) * linear_interp(2, x, y, z, 0, -1, 0))
                            t5 = coe_t_zdis * coe_t_zdis * (linear_interp(0, x, y, z, 0, 0, 1) * linear_interp(3, x, y, z, 0, 0, 1)&
                            - linear_interp(0, x, y, z, 0, 0, -1) * linear_interp(3, x, y, z, 0, 0, -1))
                            t6 = prognostic_data_old%density(z + 1, y  + 1, x + 1) - t3 - t4 - t5
                            write(*,*) t3, t4, t5, t6

                            write(*,*) "east u", linear_interp(1, x, y, z, 1, 0, 0)
                            write(*,*) "west u", linear_interp(1, x, y, z, -1, 0, 0)
                            write(*,*) "north v", linear_interp(2, x, y, z, 0, 1, 0)
                            write(*,*) "south v", linear_interp(2, x, y, z, 0, -1, 0)
                            write(*,*) "top  w", linear_interp(3, x, y, z, 0, 0, 1)
                            write(*,*) "bottom w", linear_interp(3, x, y, z, 0, 0, -1)
                        CALL EXIT
                        end if

                        result = prognostic_data_old%u(z + 1, y + 1, x + 1) * prognostic_data_old%density(z + 1, y + 1, x + 1) &
                        - coe_t_xdis * (linear_interp(0, x, y, z, 1, 0, 0) &
                        * linear_interp(1, x, y, z, 1, 0, 0) * linear_interp(1, x, y, z, 1, 0, 0) - &
                        linear_interp(0, x, y, z, -1, 0, 0) * linear_interp(1, x, y, z, -1, 0, 0) &
                        * linear_interp(1, x, y, z, -1, 0, 0)) &
                        + prognostic_data_old%density(z + 1, y + 1, x + 1) * f * &
                        prognostic_data_old%v(z + 1, y + 1, x + 1) * t_interval
                        
                        prognostic_data_new%u(z, y, x) = result / prognostic_data_new%density(z, y, x) 
                        
                        if (ABS(prognostic_data_new%u(z, y, x)) > 30 .and. debug_mk == 1) then
                            write(*,*) "z,y,x",z,y,x
                            
                            write(*,*) "prognostic_data_new%u(z, y, x):", prognostic_data_new%u(z, y, x)
                            write(*,*) "prognostic_data_old%u(z + 1, y + 1, x + 1):", &
                            prognostic_data_old%u(z + 1, y + 1, x + 1)
                            write(*,*) "prognostic_data_new%density(z, y, x)", prognostic_data_new%density(z, y, x)
                            write(*,*) "prognostic_data_old%density(z + 1, y + 1, x + 1):", &
                            prognostic_data_old%density(z + 1, y + 1, x + 1)
                            write(*,*)"------density------"
                            t3 = coe_t_xdis * (linear_interp(0, x, y, z, 1, 0, 0) * linear_interp(1, x, y, z, 1, 0, 0)&
                            - linear_interp(0, x, y, z, -1, 0, 0) * linear_interp(1, x, y, z, -1, 0, 0))
                            t4 = coe_t_ydis * (linear_interp(0, x, y, z, 0, 1, 0) * linear_interp(2, x, y, z, 0, 1, 0)&
                            - linear_interp(0, x, y, z, 0, -1, 0) * linear_interp(2, x, y, z, 0, -1, 0))
                            t5 = coe_t_zdis * coe_t_zdis * (linear_interp(0, x, y, z, 0, 0, 1) * linear_interp(3, x, y, z, 0, 0, 1)&
                            - linear_interp(0, x, y, z, 0, 0, -1) * linear_interp(3, x, y, z, 0, 0, -1))
                            t6 = prognostic_data_old%density(z + 1, y  + 1, x + 1) - t3 - t4 - t5
                            write(*,*) t3, t4, t5, t6
                            write(*,*) "east u", linear_interp(1, x, y, z, 1, 0, 0)
                            write(*,*) "west u", linear_interp(1, x, y, z, -1, 0, 0)
                            write(*,*) "north v", linear_interp(2, x, y, z, 0, 1, 0)
                            write(*,*) "south v", linear_interp(2, x, y, z, 0, -1, 0)
                            write(*,*) "top  w", linear_interp(3, x, y, z, 0, 0, 1)
                            write(*,*) "bottom w", linear_interp(3, x, y, z, 0, 0, -1)
                            CALL EXIT
                        end if

                        ! y-direction velocity
                        result = prognostic_data_old%v(z + 1, y + 1, x + 1) * prognostic_data_old%density(z + 1, y + 1, x + 1) &
                        - coe_t_ydis * (linear_interp(0, x, y, z, 0, 1, 0) * linear_interp(2, x, y, z, 0, 1, 0) &
                        * linear_interp(2, x, y, z, 0, 1, 0) - &
                        linear_interp(0, x, y, z, 0, -1, 0) * linear_interp(2, x, y, z, 0, -1, 0) &
                        * linear_interp(2, x, y, z, 0, -1, 0)) &
                        + prognostic_data_old%density(z + 1, y + 1, x + 1) * f * &
                        prognostic_data_old%u(z + 1, y + 1, x + 1) * t_interval
                        
                        prognostic_data_new%v(z, y, x) = result / prognostic_data_new%density(z, y, x) 

                        if (ABS(prognostic_data_new%v(z, y, x)) > 30 .and. debug_mk == 1) then
                            write(*,*) "z,y,x",z,y,x
                            write(*,*) "prognostic_data_new%v(z, y, x):", prognostic_data_new%v(z, y, x)
                            write(*,*) "prognostic_data_old%v(z + 1, y + 1, x + 1):", &
                            prognostic_data_old%v(z + 1, y + 1, x + 1)
                            write(*,*) "prognostic_data_new%density(z, y, x)", prognostic_data_new%density(z, y, x)
                            write(*,*) "prognostic_data_old%density(z + 1, y + 1, x + 1):", &
                            prognostic_data_old%density(z + 1, y + 1, x + 1)
                            write(*,*)"------density------"
                            t3 = coe_t_xdis * (linear_interp(0, x, y, z, 1, 0, 0) * linear_interp(1, x, y, z, 1, 0, 0)&
                            - linear_interp(0, x, y, z, -1, 0, 0) * linear_interp(1, x, y, z, -1, 0, 0))
                            t4 = coe_t_ydis * (linear_interp(0, x, y, z, 0, 1, 0) * linear_interp(2, x, y, z, 0, 1, 0)&
                            - linear_interp(0, x, y, z, 0, -1, 0) * linear_interp(2, x, y, z, 0, -1, 0))
                            t5 = coe_t_zdis * coe_t_zdis * (linear_interp(0, x, y, z, 0, 0, 1) * linear_interp(3, x, y, z, 0, 0, 1)&
                            - linear_interp(0, x, y, z, 0, 0, -1) * linear_interp(3, x, y, z, 0, 0, -1))
                            t6 = prognostic_data_old%density(z + 1, y  + 1, x + 1) - t3 - t4 - t5
                            write(*,*) t3, t4, t5, t6
                            write(*,*) "east u", linear_interp(1, x, y, z, 1, 0, 0)
                            write(*,*) "west u", linear_interp(1, x, y, z, -1, 0, 0)
                            write(*,*) "north v", linear_interp(2, x, y, z, 0, 1, 0)
                            write(*,*) "south v", linear_interp(2, x, y, z, 0, -1, 0)
                            write(*,*) "top  w", linear_interp(3, x, y, z, 0, 0, 1)
                            write(*,*) "bottom w", linear_interp(3, x, y, z, 0, 0, -1)
                      
                            CALL EXIT
                        end if

                        ! vertical velocity
                        ! result = prognostic_data_old%w(z + 1, y + 1, x + 1) * prognostic_data_old%density(z + 1, y + 1, x + 1) &
                        ! - coe_t_zdis * (linear_interp(0, x, y, z, 0, 0, 1) * linear_interp(3, x, y, z, 0, 0, 1) &
                        ! * linear_interp(3, x, y, z, 0, 0, 1) - &
                        ! linear_interp(0, x, y, z, 0, 0, -1) * linear_interp(3, x, y, z, 0, 0, -1) &
                        ! * linear_interp(3, x, y, z, 0, 0, -1)) &
                        ! - coe_t_zdis * (linear_interp(5, x, y, z, 0, 0, 1) - linear_interp(5, x, y, z, 0, 0, -1)) &
                        ! - prognostic_data_old%density(z + 1, y + 1, x + 1) * g * t_interval
                        
                        result = prognostic_data_old%w(z + 1, y + 1, x + 1) * prognostic_data_old%density(z + 1, y + 1, x + 1) &
                        - coe_t_zdis * (linear_interp(0, x, y, z, 0, 0, 1) * linear_interp(3, x, y, z, 0, 0, 1) &
                        * linear_interp(3, x, y, z, 0, 0, 1) - &
                        linear_interp(0, x, y, z, 0, 0, -1) * linear_interp(3, x, y, z, 0, 0, -1) &
                        * linear_interp(3, x, y, z, 0, 0, -1)) 
                        
                        ! write(*,*) "z,y,x",z,y,x
                        ! write(*,*) "w",coe_t_zdis * (linear_interp(0, x, y, z, 0, 0, 1) * linear_interp(3, x, y, z, 0, 0, 1) &
                        ! * linear_interp(3, x, y, z, 0, 0, 1) - &
                        ! linear_interp(0, x, y, z, 0, 0, -1) * linear_interp(3, x, y, z, 0, 0, -1) &
                        ! * linear_interp(3, x, y, z, 0, 0, -1))
                        ! write(*,*) "p", coe_t_zdis * (linear_interp(5, x, y, z, 0, 0, 1) - linear_interp(5, x, y, z, 0, 0, -1))
                        ! write(*,*) "g", prognostic_data_old%density(z + 1, y + 1, x + 1) * g
                        ! CALL EXIT
                        prognostic_data_new%w(z, y, x) = result / prognostic_data_new%density(z, y, x)

                        if (ABS(prognostic_data_new%w(z, y, x)) > 30 .and. debug_mk == 1) then
                            write(*,*) "z,y,x",z,y,x
                            write(*,*) "prognostic_data_new%w(z, y, x):", prognostic_data_new%w(z, y, x)
                            write(*,*) "prognostic_data_old%w(z + 1, y + 1, x + 1):", &
                            prognostic_data_old%w(z + 1, y + 1, x + 1)
                            write(*,*) "prognostic_data_new%density(z, y, x)", prognostic_data_new%density(z, y, x)
                            write(*,*) "prognostic_data_old%density(z + 1, y + 1, x + 1):", &
                            prognostic_data_old%density(z + 1, y + 1, x + 1)
                            write(*,*) "------w------"
                            write(*,*) "w", coe_t_zdis * (linear_interp(0, x, y, z, 0, 0, 1) * linear_interp(3, x, y, z, 0, 0, 1) &
                            * linear_interp(3, x, y, z, 0, 0, 1) - &
                            linear_interp(0, x, y, z, 0, 0, -1) * linear_interp(3, x, y, z, 0, 0, -1) &
                            * linear_interp(3, x, y, z, 0, 0, -1))
                            write(*,*) "p", coe_t_zdis * (linear_interp(5, x, y, z, 0, 0, 1) - linear_interp(5, x, y, z, 0, 0, -1))
                            write(*,*) "g", prognostic_data_old%density(z + 1, y + 1, x + 1) * g * t_interval
                            write(*,*) "------density------"
                            t3 = coe_t_xdis * (linear_interp(0, x, y, z, 1, 0, 0) * linear_interp(1, x, y, z, 1, 0, 0)&
                            - linear_interp(0, x, y, z, -1, 0, 0) * linear_interp(1, x, y, z, -1, 0, 0))
                            t4 = coe_t_ydis * (linear_interp(0, x, y, z, 0, 1, 0) * linear_interp(2, x, y, z, 0, 1, 0)&
                            - linear_interp(0, x, y, z, 0, -1, 0) * linear_interp(2, x, y, z, 0, -1, 0))
                            t5 = coe_t_zdis * coe_t_zdis * (linear_interp(0, x, y, z, 0, 0, 1) * linear_interp(3, x, y, z, 0, 0, 1)&
                            - linear_interp(0, x, y, z, 0, 0, -1) * linear_interp(3, x, y, z, 0, 0, -1))
                            t6 = prognostic_data_old%density(z + 1, y  + 1, x + 1) - t3 - t4 - t5
                            write(*,*) t3, t4, t5, t6
                            write(*,*) "east u", linear_interp(1, x, y, z, 1, 0, 0)
                            write(*,*) "west u", linear_interp(1, x, y, z, -1, 0, 0)
                            write(*,*) "north v", linear_interp(2, x, y, z, 0, 1, 0)
                            write(*,*) "south v", linear_interp(2, x, y, z, 0, -1, 0)
                            write(*,*) "top  w", linear_interp(3, x, y, z, 0, 0, 1)
                            write(*,*) "bottom w", linear_interp(3, x, y, z, 0, 0, -1)
                      
                            CALL EXIT
                        end if

                        ! theta
                        result = prognostic_data_old%theta(z + 1, y + 1, x + 1) * prognostic_data_old%density(z + 1, y + 1, x + 1) &
                        -coe_t_xdis * (linear_interp(0, x, y, z, 1, 0, 0) * linear_interp(1, x, y, z, 1, 0, 0) &
                        * linear_interp(4, x, y, z, 1, 0, 0) - &
                        linear_interp(0, x, y, z, -1, 0, 0) * linear_interp(1, x, y, z, -1, 0, 0) &
                        * linear_interp(4, x, y, z, -1, 0, 0)) &
                        -coe_t_ydis * (linear_interp(0, x, y, z, 0, 1, 0) * &
                        linear_interp(2, x, y, z, 0, 1, 0) * linear_interp(4, x, y, z, 0, 1, 0) - &
                        linear_interp(0, x, y, z, 0, -1, 0) * linear_interp(2, x, y, z, 0, -1, 0) &
                        * linear_interp(4, x, y, z, 0, -1, 0)) &
                        -coe_t_zdis * (linear_interp(0, x, y, z, 0, 0, 1) * linear_interp(3, x, y, z, 0, 0, 1) &
                        * linear_interp(4, x, y, z, 0, 0, 1) - &
                        linear_interp(0, x, y, z, 0, 0, -1) * linear_interp(3, x, y, z, 0, 0, -1) * &
                        linear_interp(4, x, y, z, 0, 0, -1))
                        
                        prognostic_data_new%theta(z, y, x) = result / prognostic_data_new%density(z, y, x)
                    end do
                end do
            end do
            ! 2:z_end + 1,2:y_end + 1,2:x_end + 1
            prognostic_data_old%density(2:z_end+1, 2:y_end+1, 2:x_end+1) = prognostic_data_new%density
            prognostic_data_old%u(2:z_end+1, 2:y_end+1, 2:x_end+1) = prognostic_data_new%u
            prognostic_data_old%v(2:z_end+1, 2:y_end+1, 2:x_end+1) = prognostic_data_new%v
            prognostic_data_old%w(2:z_end+1, 2:y_end+1, 2:x_end+1) = prognostic_data_new%w
            prognostic_data_old%theta(2:z_end+1, 2:y_end+1, 2:x_end+1) = prognostic_data_new%theta

            ! write data to NWFM_out.nc
            call write_data2nc(t_rec)

            ! debug
            ! do z = 1, z_end
            !     do y = 1, y_end
            !         write(*,*) "---z, y:",z,",",y,"---"
            !         write(*,*) prognostic_data_new%v(z,y,:)
            !     end do    
            ! end do

            ! EXIT
        end do
        call close_nc()
        call close_force_nc()
    contains
        function linear_interp(v_id, xx, yy, zz, x_mk, y_mk, z_mk) result(retval)
            integer :: v_id, xx, yy, zz, x_mk, y_mk, z_mk, x, y, z
            real :: retval
            x = xx + 1
            y = yy + 1
            z = zz + 1

            if (v_id == 0) then
                retval = (prognostic_data_old%density(z + 1 * z_mk, y + 1 * y_mk, x + 1 * x_mk) &
                + prognostic_data_old%density(z, y, x))
            else if (v_id == 1) then
                retval = (prognostic_data_old%u(z + 1 * z_mk, y + 1 * y_mk, x + 1 * x_mk) + prognostic_data_old%u(z, y, x))
            else if (v_id == 2) then
                retval = (prognostic_data_old%v(z + 1 * z_mk, y + 1 * y_mk, x + 1 * x_mk) + prognostic_data_old%v(z, y, x))
            else if (v_id == 3) then
                retval = (prognostic_data_old%w(z + 1 * z_mk, y + 1 * y_mk, x + 1 * x_mk) + prognostic_data_old%w(z, y, x))
            else if (v_id == 4) then
                retval = (prognostic_data_old%theta(z + 1 * z_mk, y + 1 * y_mk, x + 1 * x_mk) + prognostic_data_old%theta(z, y, x))
            else if (v_id == 5) then
                retval = (aux_data%pressure(z + 1 * z_mk, y + 1 * y_mk, x + 1 * x_mk) + aux_data%pressure(z, y, x))
            end if
              
            retval = retval * 0.5  
        end function linear_interp
        
        function linear_interp_b(v_id, x, y) result(retval)
            integer, intent(in) :: v_id, x, y
            real :: retval
            
            if (v_id == 0) then
                retval = force_data_bottom%density(y, x) + prognostic_data_old%density(1, y, x)
            else if (v_id == 1) then
                retval = (force_data_bottom%u(y, x) + prognostic_data_old%u(1, y, x))
            else if (v_id == 2) then
                retval = (force_data_bottom%v(y, x) + prognostic_data_old%v(1, y, x))
            else if (v_id == 3) then
                retval = (force_data_bottom%w(y, x) + prognostic_data_old%w(1, y, x))
            else if (v_id == 4) then
                retval = (force_data_bottom%theta(y, x) + prognostic_data_old%theta(1, y, x))
            ! else if (v_id == 5) then
            !     retval = (force_data_bottom%u(y, x) + prognostic_data_old%u(1, y, x))
            end if
            retval = retval * 0.5  
            
        end function linear_interp_b

        function linear_interp_t(v_id, x, y) result(retval)
            integer, intent(in) :: v_id, x, y
            real :: retval
            
            if (v_id == 0) then
                retval = force_data_top%density(y, x) + prognostic_data_old%density(grid_para%z_nums, y, x)
            else if (v_id == 1) then
                retval = (force_data_top%u(y, x) + prognostic_data_old%u(grid_para%z_nums, y, x))
            else if (v_id == 2) then
                retval = (force_data_top%v(y, x) + prognostic_data_old%v(grid_para%z_nums, y, x))
            else if (v_id == 3) then
                retval = (force_data_top%w(y, x) + prognostic_data_old%w(grid_para%z_nums, y, x))
            else if (v_id == 4) then
                retval = (force_data_top%theta(y, x) + prognostic_data_old%theta(grid_para%z_nums, y, x))
            ! else if (v_id == 5) then
            !     retval = (force_data_bottom%u(y, x) + prognostic_data_old%u(1, y, x))
            end if
            retval = retval * 0.5  
            
        end function linear_interp_t
    end subroutine run
    ! function convertHeight2Pressure(arg) result(retval)
    !     integer, intent(in) :: arg
    !     integer :: retval
    
        
    ! end function convertHeight2Pressure
    
    ! function convertTemprature2Theta(arg) result(retval)
    !     integer, intent(in) :: arg
    !     integer :: retval
    
        
    ! end function convertTemprature2Theta

    ! function getDensityFromPressureAndTemprature(arg) result(retval)
    !     integer, intent(in) :: arg
    !     integer :: retval
    
        
    ! end function getDensityFromPressureAndTemprature

end module dynamicSolver


! ! traverse boundary nodes
            
!             ! bottom bodundary
! z = 1
! do y = 2, y_end - 1
!     do x = 2, x_end - 1
!         prognostic_data_new%density(z, y, x) = prognostic_data_old%density(z, y, x)&
!         - coe_t_xdis * (linear_interp(0, x, y, z, 1, 0, 0) * linear_interp(1, x, y, z, 1, 0, 0) &
!         - linear_interp(0,x,y,z,-1,0,0)*linear_interp(1,x,y,z,-1,0,0))&
!         - coe_t_ydis * (linear_interp(0, x, y, z, 0, 1, 0) * linear_interp(1, x, y, z, 0, 1, 0) &
!         - linear_interp(0,x,y,z,0,-1,0)*linear_interp(1,x,y,z,0,-1,0))&
!         - coe_t_zdis * (linear_interp(0, x, y, z, 0, 0, 1) * linear_interp(1, x, y, z, 0, 0, 1) &
!         - linear_interp_b(0,x,y)*linear_interp_b(1,x,y))
!     end do
! end do 

! ! top bodundary
! z = z_end
! do y = 2, y_end - 1
!     do x = 2, x_end - 1
!         prognostic_data_new%density(z, y, x) = prognostic_data_old%density(z, y, x)&
!         - coe_t_xdis * (linear_interp(0, x, y, z, 1, 0, 0) * linear_interp(1, x, y, z, 1, 0, 0) &
!         - linear_interp(0,x,y,z,-1,0,0)*linear_interp(1,x,y,z,-1,0,0))&
!         - coe_t_ydis * (linear_interp(0, x, y, z, 0, 1, 0) * linear_interp(1, x, y, z, 0, 1, 0) &
!         - linear_interp(0,x,y,z,0,-1,0)*linear_interp(1,x,y,z,0,-1,0))&
!         - coe_t_zdis * (linear_interp_t(0,x,y)*linear_interp_t(1,x,y) &
!         - linear_interp(0,x,y,z,0,0,-1)*linear_interp(1,x,y,z,0,0,-1))
!     end do
! end do


 ! do y = 2, y_end+1
            !     do x = 2, x_end+1
            !         ! bottom
            !         prognostic_data_old%density(1, y, x) = force_data_bottom%density(y-1,x-1)
            !         prognostic_data_old%u(1, y, x) = force_data_bottom%u(y-1,x-1)
            !         prognostic_data_old%v(1, y, x) = force_data_bottom%v(y-1,x-1)
            !         prognostic_data_old%w(1, y, x) = force_data_bottom%w(y-1,x-1)
            !         prognostic_data_old%theta(1, y, x) = force_data_bottom%theta(y-1,x-1)
            !         aux_data%pressure(1, y, x) = force_data_bottom%pressure(y-1,x-1)

            !         ! top
            !         prognostic_data_old%density(grid_para%z_nums + 2, y, x) = force_data_top%density(y-1, x-1)
            !         prognostic_data_old%u(grid_para%z_nums + 2, y, x) = force_data_top%u(y-1, x-1)
            !         prognostic_data_old%v(grid_para%z_nums + 2, y, x) = force_data_top%v(y-1, x-1)
            !         prognostic_data_old%w(grid_para%z_nums + 2, y, x) = force_data_top%w(y-1, x-1)
            !         prognostic_data_old%theta(grid_para%z_nums + 2, y, x) = force_data_top%theta(y-1, x-1)
            !         aux_data%pressure(grid_para%z_nums + 2, y, x) = force_data_top%pressure(y-1, x-1)
            !     end do
            ! end do

            ! do z = 2, z_end+1
            !     do y = 2, y_end+1
            !         ! west
            !         prognostic_data_old%density(z, y, 1) = force_data_west%density
            !         prognostic_data_old%u(z, y, 1) = force_data_west%u
            !         prognostic_data_old%v(z, y, 1) = force_data_west%v
            !         prognostic_data_old%w(z, y, 1) = force_data_west%w
            !         prognostic_data_old%theta(z, y, 1) = force_data_west%theta
            !         aux_data%pressure(z, y, 1) = force_data_west%pressure
            !         ! east
            !         prognostic_data_old%density(z, y,  x_end + 2) = force_data_east%density
            !         prognostic_data_old%u(z, y,  x_end + 2) = force_data_east%u
            !         prognostic_data_old%v(z, y,  x_end + 2) = force_data_east%v
            !         prognostic_data_old%w(z, y,  x_end + 2) = force_data_east%w
            !         prognostic_data_old%theta(z, y,  x_end + 2) = force_data_east%theta
            !         aux_data%pressure(z, y,  x_end + 2) = force_data_east%pressure
            !     end do
            ! end do
            
            ! do z = 2, z_end+1
            !     do x = 2, x_end+1
            !         ! south
            !         prognostic_data_old%density(z, 1, x) = force_data_south%density(z-1,x-1)
            !         prognostic_data_old%u(z, 1, x) = force_data_south%u(z-1,x-1)
            !         prognostic_data_old%v(z, 1, x) = force_data_south%v(z-1,x-1)
            !         prognostic_data_old%w(z, 1, x) = force_data_south%w(z-1,x-1)
            !         prognostic_data_old%theta(z, 1, x) = force_data_south%theta(z-1,x-1)
            !         aux_data%pressure(z, 1, x) = force_data_south%pressure(z-1,x-1)
            !         ! north
            !         prognostic_data_old%density(z, grid_para%y_nums + 2, x) = force_data_north%density(z-1,x-1)
            !         prognostic_data_old%u(z, grid_para%y_nums + 2, x) = force_data_north%u(z-1,x-1)
            !         prognostic_data_old%v(z, grid_para%y_nums + 2, x) = force_data_north%v(z-1,x-1)
            !         prognostic_data_old%w(z, grid_para%y_nums + 2, x) = force_data_north%w(z-1,x-1)
            !         prognostic_data_old%theta(z, grid_para%y_nums + 2, x) = force_data_north%theta(z-1,x-1)
            !         aux_data%pressure(z, grid_para%y_nums + 2, x) = force_data_north%pressure(z-1,x-1)
            !     end do
            ! end do
            ! south
            ! write(*,*) prognostic_data_old%density
            ! EXIT
            ! traverse every single point except boundaries