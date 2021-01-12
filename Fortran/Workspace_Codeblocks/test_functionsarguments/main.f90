! =========
! PROGRAM TO TEST THE PERFORMANCE OF PROCEDURES ACCORDING TO HOW ARGUMENTS ARE PASSED
! TEST DONE WITH FUNCTIONS PERFORMING VECTORIAL DOT PRODUCT
! FASTEST: PASS ONLY ARRAYS, NOT DIMENSION, WHICH IS KNOWN AS A PARAMETER INSIDE THE FUNCTION
!
! =========

PROGRAM hello_world
    USE test_module, ONLY : VdotV_1, VdotV_2, VdotV_3
    IMPLICIT NONE
    INTEGER, PARAMETER :: nSize = 100000
    REAL(8) :: a(nSize), b(nSize), c(nSize)
    INTEGER :: i,j,k,l
    REAL(8) :: start, finish, dt
    INTEGER, PARAMETER :: n = 10000

    WRITE(*,*) 'Hello World!'

    DO i=1,nSize
        a(i) = i
        b(i) = 2*i
    END DO

    ! ===================
    WRITE(*,*) '========='

    CALL CPU_TIME(start)

    WRITE(*,*) a(nSize)
    WRITE(*,*) b(nSize)

    DO i=1,n
        c = VdotV_1(a,b,nSize)
    END DO

    WRITE(*,*) c(nSize)

    CALL CPU_TIME(finish)
    dt = finish - start
    WRITE(*,*) dt

    WRITE(*,*) '========='
    ! ===================
    WRITE(*,*) '========='

    CALL CPU_TIME(start)

    WRITE(*,*) a(nSize)
    WRITE(*,*) b(nSize)

    DO i=1,n
        c = VdotV_2(a,b,nSize)
    END DO

    WRITE(*,*) c(nSize)

    CALL CPU_TIME(finish)
    dt = finish - start
    WRITE(*,*) dt

    WRITE(*,*) '========='
    ! ===================
    WRITE(*,*) '========='

    CALL CPU_TIME(start)

    WRITE(*,*) a(nSize)
    WRITE(*,*) b(nSize)

    DO i=1,n
        c = VdotV_3(a,b)
    END DO

    WRITE(*,*) c(nSize)

    CALL CPU_TIME(finish)
    dt = finish - start
    WRITE(*,*) dt

    WRITE(*,*) '========='
    ! ===================


    PRINT *, "Hello World!"

END PROGRAM

