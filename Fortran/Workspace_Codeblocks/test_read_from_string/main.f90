program main 
implicit none 
character(len=120) :: s 
real(8) :: a(2,2) 

s = "1.1d0   1.2d0   1.3d0 1.4d0" 

read(s, *) a 
write(*,*) s
write(*,*) a
write(*,*) a(1,1), a(1,2) 
write(*,*) a(2,1), a(2,2)

end program main 
