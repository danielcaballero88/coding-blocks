program test_cycle 
    implicit none 
    integer i 
    
    do i=1,10 
        if ( (i==3) .or. (i==10) ) then 
            cycle 
        end if 
        write(*,*) i 
    end do 

end program 
