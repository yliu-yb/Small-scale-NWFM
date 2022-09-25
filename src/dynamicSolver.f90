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
    implicit none
    use customTypes

    type(time_para) :: time_para
    type(grid_para) :: grid_para
    type(initial_data) :: initial_datas
    type(force_data) :: force_data_bottom
    type(force_data) :: force_data_top
    type(force_data) :: force_data_west
    type(force_data) :: force_data_east
    type(force_data) :: force_data_south
    type(force_data) :: force_data_north
    type(prognostic_data) :: prognostic_data_old
    type(prognostic_data) :: prognostic_data_new

    ! prognostic_data_old = initial_data
    ! prognostic_data_old = prognostic_data_new

contains
    function convertHeight2Pressure(arg) result(retval)
        integer, intent(in) :: arg
        integer :: retval
    
        
    end function convertHeight2Pressure
    
    function convertTemprature2Theta(arg) result(retval)
        integer, intent(in) :: arg
        integer :: retval
    
        
    end function convertTemprature2Theta

    function getDensityFromPressureAndTemprature(arg) result(retval)
        integer, intent(in) :: arg
        integer :: retval
    
        
    end function getDensityFromPressureAndTemprature

end module dynamicSolver