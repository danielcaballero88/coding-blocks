PROGRAM hello
    USE MOD_Intent, ONLY : AsignarValores
    IMPLICIT NONE
    INTEGER, PARAMETER :: nSize = 5
    REAL(8) :: u(nSize), v(nSize)
    INTEGER :: i

    PRINT *, "Hello World!"

    DO i=1,nSize
        u(i) = i
        v(i) = 2.d0*i
    END DO

    CALL AsignarValores(u, v, nSize)

    PRINT *, u
    PRINT *, v

END PROGRAM

