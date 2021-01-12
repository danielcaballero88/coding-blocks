program main 
    implicit none 
    integer :: i=1
    
    select case (i) 
    case (1) 
        write(*,*) "1: ", i 
    case default 
        write(*,*) "default: ", i
    end select

end program main
