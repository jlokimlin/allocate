program alloc

implicit none

integer, parameter :: dp=kind(0.d0)

do while (.true.)
    call f()
end do

contains

subroutine f()
real(dp), pointer :: A(:)
call test(A)
end subroutine

subroutine test(A)
real(dp), pointer, intent(out) :: A(:)
allocate(A(10000000))
A(1) = 5
end subroutine

end program
