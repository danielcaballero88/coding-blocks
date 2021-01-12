MODULE MOD_Intent

IMPLICIT NONE
PRIVATE

PUBLIC :: AsignarValores

CONTAINS

SUBROUTINE AsignarValores(u, v, n)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: n
    REAL(8), INTENT(OUT) :: u(n), v(n)
    INTEGER :: i

    DO i=1,n
        u(i) = v(i)
    END DO


END SUBROUTINE

END MODULE MOD_Intent

