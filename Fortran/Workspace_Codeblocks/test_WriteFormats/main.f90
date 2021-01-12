program main 
implicit none 
integer :: i 
real(8) :: r

! integer
i=1
r = 2.d0
write(*,"(I10)") i      ! "         1"
write(*,"(I10.10)") i   ! "0000000001"
write(*,"(I10.8)") i    ! "  00000001"
write(*, "(I10, SP, I10, SS, I10)") i, i, i

write(*,"(E20.8E4)") r

! float

end program main 
