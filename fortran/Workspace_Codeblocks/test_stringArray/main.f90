SUBROUTINE AssignNames(nSize, NamesArray)
    IMPLICIT NONE
    INTEGER, INTENT(IN) :: nSize
    CHARACTER(LEN=120), INTENT(OUT) :: NamesArray(nSize)
    INTEGER :: i

    DO i=1,nSize
        WRITE(NamesArray(i),*) i
    END DO

END SUBROUTINE


PROGRAM Hola
    IMPLICIT NONE
    CHARACTER(LEN=120), ALLOCATABLE :: Nombres(:)
    INTEGER :: n=10
    INTEGER :: i

    PRINT *, "Hola Mundo!"

    ALLOCATE (Nombres(n))

    CALL AssignNames(n,Nombres)

    DO i=1,n
        print *, Nombres(i)
    END DO


END PROGRAM

