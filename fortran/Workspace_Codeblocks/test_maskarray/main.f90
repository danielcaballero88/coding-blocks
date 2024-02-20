
program hello
    implicit none
    real(8) :: v(10), A(3,3), b(4)
    logical :: mask(3,3)
    integer :: i, j


    v = [21, 22, 23, 24, 25, 26, 27, 28, 29, 30]

    print *, PACK(v,(v>24))
    print *, minloc(v, dim=1, mask=(v>=24.5))
    print *, minval(v, dim=1, mask=(v>=24.5))

    data mask / .TRUE., .TRUE., .FALSE., &
                .FALSE., .FALSE., .FALSE., &
                .FALSE., .TRUE., .TRUE. /

    data A / 1,2,3, &
             4,5,6, &
             7,8,9 /

    b = PACK(A, mask)

    DO i=1,3
        write(*,*) mask(i,:)
    END DO

    DO i=1,3
        write(*,*) A(i,:)
    END DO

    DO i=1,4
        write(*,*) b(i)
    END DO

end program

