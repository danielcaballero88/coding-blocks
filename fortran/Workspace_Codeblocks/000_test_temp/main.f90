program main
    implicit none 
    integer :: a(4) 
    integer :: i

    i = 3 
    a = [1,2,3,4] 

    print *, any(a==i)

    

end program main
