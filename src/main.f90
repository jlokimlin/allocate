program main

    use, intrinsic :: iso_fortran_env, only: &
        wp     => REAL64, &
        ip     => INT32, &
        stdout => OUTPUT_UNIT, &
        compiler_version, &
        compiler_options

    use module_allocate, only: &
        alloc => allocate_one_dimensional_array

    ! Explicit typing only
    implicit none

    !--------------------------------------------------------------------------------
    ! Dictionary
    !--------------------------------------------------------------------------------
    integer (ip) :: array_size
    integer (ip) :: i, j !! Counters
    integer (ip) :: start_time, end_time
    integer (ip) :: total_time_without_allocation, total_time_with_allocation
    !--------------------------------------------------------------------------------

    ! Initialize constants
    total_time_without_allocation = 0_ip
    total_time_with_allocation = 0_ip

    do j = 1,10

        call system_clock( count = start_time )

        do i = 1,1000000
            array_size = get_random_integer()
            call alloc( n = -1 )
        end do

        call system_clock( count = end_time )
        total_time_without_allocation = total_time_without_allocation + (end_time-start_time)

        call system_clock( count = start_time )

        do i = 1,1000000
            array_size = get_random_integer()
            call alloc( n = array_size )
        end do

        call system_clock( count = end_time )
        total_time_with_allocation = total_time_with_allocation + (end_time-start_time)
    end do

    ! Print results to console
    write( stdout, '(A)') ' '
    write( stdout,'(A,I4)') 'Total time without allocation: ', total_time_without_allocation
    write( stdout,'(A,I4)') 'Total time with allocation:    ', total_time_with_allocation
    write( stdout, '(A)') ' '
    write( stdout, '(4A)') &
        'This program was compiled by ', compiler_version(), &
        ' using the options '       , compiler_options()

contains
    !
    !*****************************************************************************************
    !
    function get_random_integer() result (return_value)
        !--------------------------------------------------------------------------------
        ! Dictionary: calling arguments
        !--------------------------------------------------------------------------------
        integer (ip) :: return_value
        !--------------------------------------------------------------------------------
        ! Dictionary: local variables
        !--------------------------------------------------------------------------------
        real (wp)    :: random_scalar
        !--------------------------------------------------------------------------------

        call random_number( random_scalar )

        return_value = int( 1.0e+5 * random_scalar, kind = ip)

    end function
    !
    !*****************************************************************************************
    !
end program main
