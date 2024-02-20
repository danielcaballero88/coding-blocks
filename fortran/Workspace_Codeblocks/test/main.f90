program main 
    implicit none 
    integer :: v(4) 
    
    v = [1,2,3,4] 
    
    write(*,*) v
    
    call change_val(v(2)) 
    
    write(*,*) v

end program main 

subroutine change_val(a) 
    implicit none
    integer, intent(inout) :: a
    
    a = a+1

end subroutine change_val
