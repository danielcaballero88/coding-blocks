PROGRAM hello
    USE MOD_Asignar
    IMPLICIT NONE
    INTEGER, PARAMETER :: n = 5
    INTEGER, ALLOCATABLE :: u(:,:)
    TYPE(mytype) :: v(1000)
    INTEGER :: i

    PRINT *, "Hello World!"

    ALLOCATE( u(2,n) )

    DO i=1,n
        v(i)%r = [+i, -i]
    END DO

    DO i=1,n
        WRITE(*,*) v(i)%r
    END DO

    DO i=1,n
        u(:,i) = v(i)%r(:)
    END DO

    DO i=1,n
        WRITE(*,*) u(:,i)
    END DO

    WRITE(*,*) "---"
    WRITE(*,*) u(2:2,1)

END PROGRAM

