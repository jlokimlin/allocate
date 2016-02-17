module module_allocate

    use, intrinsic :: iso_fortran_env, only: &
        wp => REAL64, &
        ip => INT32

    ! Explicit typing only
    implicit none

     ! Everything is private unless stated otherwise
    private
    public :: allocate_one_dimensional_array

contains
    !
    !*****************************************************************************************
    !
    subroutine allocate_one_dimensional_array( n )
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        integer (ip), intent (in) :: n
        !--------------------------------------------------------------------------------
        ! Dictionary: local variables
        !--------------------------------------------------------------------------------
        real (wp), allocatable :: array(:)
        !--------------------------------------------------------------------------------

        if ( n > 0 ) then
            allocate( array(n) )
        end if

    end subroutine
    !
    !*****************************************************************************************
    !
end module module_allocate
