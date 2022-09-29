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
        integer         :: i, t_nums

        ! call create_nc()
        ! t_nums = time_para%run_hours * 3600 / time_para%t_interval
        
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
        
        call get_initial_from_nc()


        
    end subroutine IO_test

    subroutine run()
        integer :: t, t_end, t_interval, count
        integer :: x, y, z, x_end, y_end, z_end, x_interval, y_interval, z_interval
        real :: coe_t_xdis, coe_t_ydis, coe_t_zdis
        real :: x_tendency, y_tendency, z_tendency
        real :: f, g
        real :: result
        t_end = time_para%run_hours * 3600
        t_interval = time_para%t_interval
        x_interval = grid_para%x_interval
        y_interval = grid_para%y_interval
        z_interval = grid_para%z_interval
        
        ! Geospin parameter
        f = 1
        
        ! Gravitational acceleration  
        g = 9.8

        ! common coeficients
        coe_t_xdis = t_interval / (x_interval)
        coe_t_ydis = t_interval / (y_interval)
        coe_t_zdis = t_interval / (z_interval)

        count = 0
        do t = 1, t_end, t_interval
            count = count + 1
            ! get force data at time t from file
            
            
            ! traverse every single point except boundaries
            do z = 2, z_end - 1
                do y = 2, y_end - 1 
                    do x = 2, x_end - 1
                        ! density
                        prognostic_data_new%density(z, y, x) = prognostic_data_old%density(z, y, x) &
                        - coe_t_xdis * (linear_interp(0, x, y, z, 1, 0, 0) * linear_interp(1, x, y, z, 1, 0, 0) &
                        - linear_interp(0,x,y,z,-1,0,0)*linear_interp(1,x,y,z,-1,0,0))&
                        - coe_t_ydis * (linear_interp(0, x, y, z, 0, 1, 0) * linear_interp(1, x, y, z, 0, 1, 0) &
                        - linear_interp(0,x,y,z,0,-1,0)*linear_interp(1,x,y,z,0,-1,0))&
                        - coe_t_zdis * (linear_interp(0, x, y, z, 0, 0, 1) * linear_interp(1, x, y, z, 0, 0, 1) &
                        - linear_interp(0,x,y,z,0,0,-1)*linear_interp(1,x,y,z,0,0,-1))
                        
                        ! x-direction velocity
                        result = prognostic_data_old%u(z, y, x) * prognostic_data_old%density(z, y, x) &
                        - coe_t_xdis * (linear_interp(0, x, y, z, 1, 0, 0) &
                        * linear_interp(1, x, y, z, 1, 0, 0) * linear_interp(1, x, y, z, 1, 0, 0) - &
                        linear_interp(0, x, y, z, -1, 0, 0) * linear_interp(1, x, y, z, -1, 0, 0) &
                        * linear_interp(1, x, y, z, -1, 0, 0)) &
                        + prognostic_data_old%density(z, y, x) * f * prognostic_data_old%v(z, y, x) * t_interval
                        
                        prognostic_data_new%u(z, y, x) = result / prognostic_data_new%density(z, y, x) 

                        ! y-direction velocity
                        result = prognostic_data_old%v(z, y, x) * prognostic_data_old%density(z, y, x) &
                        - coe_t_ydis * (linear_interp(0, x, y, z, 0, 1, 0) * linear_interp(2, x, y, z, 0, 1, 0) &
                        * linear_interp(2, x, y, z, 0, 1, 0) - &
                        linear_interp(0, x, y, z, 0, -1, 0) * linear_interp(2, x, y, z, 0, -1, 0) &
                        * linear_interp(2, x, y, z, 0, -1, 0)) &
                        + prognostic_data_old%density(z, y, x) * f * prognostic_data_old%u(z, y, x) * t_interval
                        
                        prognostic_data_new%v(z, y, x) = result / prognostic_data_new%density(z, y, x) 

                        ! vertical velocity
                        result = prognostic_data_old%w(z, y, x) * prognostic_data_old%density(z, y, x) &
                        - coe_t_zdis * (linear_interp(0, x, y, z, 0, 0, 1) * linear_interp(3, x, y, z, 0, 0, 1) &
                        * linear_interp(3, x, y, z, 0, 0, 1) - &
                        linear_interp(0, x, y, z, 0, 0, -1) * linear_interp(3, x, y, z, 0, 0, -1) &
                        * linear_interp(3, x, y, z, 0, 0, -1)) &
                        - coe_t_zdis * (linear_interp(5, x, y, z, 0, 0, 1) - linear_interp(5, x, y, z, 0, 0, -1)) &
                        - prognostic_data_old%density(z, y, x) * g * t_interval
                        
                        prognostic_data_new%v(z, y, x) = result / prognostic_data_new%density(z, y, x)

                        ! theta
                        result = prognostic_data_old%theta(z, y, x) * prognostic_data_old%density(z, y, x) &
                        -coe_t_xdis * (linear_interp(0, x, y, z, 1, 0, 0) * linear_interp(1, x, y, z, 1, 0, 0) &
                        * linear_interp(4, x, y, z, 1, 0, 0) - &
                        linear_interp(0, x, y, z, -1, 0, 0) * linear_interp(1, x, y, z, -1, 0, 0) &
                        * linear_interp(4, x, y, z, -1, 0, 0)) &
                        -coe_t_ydis * (linear_interp(0, x, y, z, 0, 1, 0) * &
                        linear_interp(2, x, y, z, 0, 1, 0) * linear_interp(4, x, y, z, 0, 1, 0) - &
                        linear_interp(0, x, y, z, 0, -1, 0) * linear_interp(2, x, y, z, 0, -1, 0) &
                        * linear_interp(4, x, y, z, 0, -1, 0)) &
                        -coe_t_zdis * (linear_interp(0, x, y, z, 0, 0, 1) * linear_interp(1, x, y, z, 0, 0, 1) &
                        * linear_interp(4, x, y, z, 0, 0, 1) - &
                        linear_interp(0, x, y, z, 0, 0, -1) * linear_interp(3, x, y, z, 0, 0, -1) * &
                        linear_interp(4, x, y, z, 0, 0, -1))
                        
                        prognostic_data_new%theta(z, y, x) = result / prognostic_data_new%density(z, y, x)
                    end do
                end do
            end do
            prognostic_data_old = prognostic_data_new
            ! write data to NWFM_out.nc
            
        end do
    contains
        function linear_interp(v_id, x, y, z, x_mk, y_mk, z_mk) result(retval)
            integer, intent(in) :: v_id, x, y, z, x_mk, y_mk, z_mk
            real :: retval
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