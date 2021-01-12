MODULE MOD_Asignar

IMPLICIT NONE
!PRIVATE
!PUBLIC :: AsignarDimension10, DarValores10

TYPE mytype
    INTEGER :: r(2)
END TYPE

CONTAINS

!SUBROUTINE DarValoresYaAsignado(v)
!    IMPLICIT NONE
!    REAL(8)
!
!END SUBROUTINE

SUBROUTINE DarValores10(v)
    IMPLICIT NONE
    REAL(8), ALLOCATABLE, INTENT(OUT) :: v(:)
    INTEGER :: i

    CALL AsignarDimension10(v)
    DO i=1,SIZE(v)
        v(i) = v(i) + i
    END DO

    WRITE(*,*) v

END SUBROUTINE

SUBROUTINE AsignarDimension10(v)
    IMPLICIT NONE
    REAL(8), ALLOCATABLE :: v(:)

    IF (ALLOCATED(v)) DEALLOCATE(v)
    ALLOCATE( v(10) )
    v = 1
    WRITE(*,*) 'size of v: ', SIZE(v)

END SUBROUTINE

END MODULE MOD_Asignar
