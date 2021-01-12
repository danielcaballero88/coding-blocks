
program hello
    implicit none
    real(8), parameter :: pi = 4.d0 * datan(1.d0)
    CHARACTER(LEN=60) :: longString, formato
    INTEGER :: i,m
    INTEGER, ALLOCATABLE :: v(:)
    integer :: fid
    real(8) :: r

    CALL writestring()

    m=7
    ALLOCATE( v(m) )
    DO i=1,m
        v(i) = i**2
    END DO
    WRITE(*,*) v
    CALL write_formato_variable(m,v)

    fid = 99
    OPEN(unit=fid,file="test.txt",status="replace")
    write(fid,'(E20.8E3)') pi
    close(fid)

    OPEN(unit=fid,file="test.txt",status="old")
    read(fid,'(E10.4E2)') r
    CLOSE(fid)

    write(*,*) pi
    write(*,*) r
    write(*,'(E10.4E2)') pi
    write(*,'(E10.4E2)') r


end program

subroutine writestring()
    implicit none
    CHARACTER(LEN=60) :: longString, formato
    INTEGER :: length

    longString = "Hello "
    length = MAX(LEN_TRIM(longString), LEN_TRIM('World!'))

    WRITE(*,'("(A", I0, ")")') length

    WRITE(formato,'(A1,I0,A1,I0,A1)') "(",2,"A",length,")"
    WRITE(*,*) formato

!    WRITE(formato,'("(", I0, "A", I0, ")")') 2, length
!    WRITE(*,*) formato

    WRITE(*,formato) longString, 'World!'
end subroutine

subroutine write_formato_variable(n,u)
    implicit none
    INTEGER, INTENT(IN) :: n
    INTEGER, INTENT(IN) :: u(n)
    CHARACTER(LEN=60) :: formato

    WRITE(formato,'(A1,I0,A3)') "(",n,"I4)"
    WRITE(*,*) formato

    WRITE(*,formato) u
end subroutine

