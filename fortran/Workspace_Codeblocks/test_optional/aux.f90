module aux 

contains 

subroutine imprimir(n) 
    integer, intent(in), optional :: n 
    integer :: nL

    ! dando un valor por defecto
    if (present(n)) then ; nL=n ; else ; nL=100 ; end if
    write(*,*) nL

end subroutine imprimir 

end module
