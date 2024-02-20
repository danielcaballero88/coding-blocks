SUBROUTINE WriteMatrix(A, nDim)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nDim
    INTEGER, INTENT(IN) :: A(nDim,nDim)
    INTEGER :: i

    DO i=1,nDim
        WRITE(*,*) A(i,:)
    END DO

END SUBROUTINE

PROGRAM hello
    IMPLICIT NONE
    INTEGER :: f(10)
    INTEGER :: i

    f = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

    CALL WriteMatrix(f(3), 2)


END PROGRAM

