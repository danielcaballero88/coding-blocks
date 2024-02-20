! ==============================================================================
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
! ==============================================================================
!
! MODULE: TODO_module_name
!
! AUTHOR:
! TODO_name_and_affiliation
!
! DESCRIPTION:
! TODO_describe_purpose_of_module.
! ==============================================================================
MODULE mod_algebra

    ! --------------------------------------------------------------------------
    IMPLICIT NONE
    PRIVATE ! NOTE_avoid_public_variables_if_possible
    ! --------------------------------------------------------------------------
    PUBLIC :: ComputeFromCurve
    PUBLIC :: IntegCumTrapz
    PUBLIC :: ProductoTensorial_nDim2
    PUBLIC :: DeltaArray
    PUBLIC :: DiferenciaRelativa
    ! --------------------------------------------------------------------------

! ==============================================================================
CONTAINS
! ==============================================================================


    ! ==========================================================================
    FUNCTION ComputeFromCurve(x, nPoints, xArray, yArray, extrapolar_IN) RESULT(y)
        ! ----------------------------------------------------------------------
        ! Esta funcion calcula el valor y=f(x) de una curva f(x) discreta
        ! INPUT:
        !       x= valor de variable independiente
        !       nPoints= numero de puntos en la curva discreta
        !       xArray= valores de x de la curva discreta
        !       yArray= valores de y de la curva discreta
        !       extrapolar_IN= opcional, indica si se extrapola en caso de que
        !                      x caiga fuera de xArray. Se interpola con los
        !                      valores de los extremos
        ! ----------------------------------------------------------------------
        IMPLICIT NONE
        ! ----------------------------------------------------------------------
        REAL(8), INTENT(IN) :: x
        INTEGER, INTENT(IN) :: nPoints
        REAL(8), INTENT(IN) :: xArray(3)
        REAL(8), INTENT(IN) :: yArray(nPoints)
        LOGICAL, OPTIONAL, INTENT(IN) :: extrapolar_IN
        ! ----------------------------------------------------------------------
        REAL(8) :: y
        ! ----------------------------------------------------------------------
        LOGICAL :: extrapolar
        REAL(8) :: x_min, x_max, x_del
        REAL(8) :: x1, x2, y1, y2
        LOGICAL :: fdi1, fdi2 ! logicals "fuera de intervalo"
        INTEGER :: i_pre ! indice del valor previo a x en xArray
        REAL(8) :: slope
        ! ----------------------------------------------------------------------

        ! ______________________________________________________________________
        extrapolar = .FALSE.
        IF ( PRESENT(extrapolar_IN) ) THEN
            extrapolar = extrapolar_IN
        END IF
        ! ______________________________________________________________________
        x_min = xArray(1)
        x_max = xArray(2)
        x_del = xArray(3)
        ! ______________________________________________________________________
        ! booleanos para fuera de intervalo (fdi)
        fdi1 = ( x < x_min )
        fdi2 = ( x > x_max )
        IF ( (fdi1) .OR. (fdi2) ) THEN
            IF ( extrapolar) THEN
                IF (fdi1) THEN
                    y = yArray(1)
                ELSE
                    y = yArray(nPoints)
                END IF
            ELSE
                WRITE(*,*) 'Error, trying to compute out of bounds - ComputeFromCurve()'
                WRITE(*,*) 'xmin, xmax, x: ', x_min, x_max, x
                STOP
            END IF
        ELSE
            ! __________________________________________________________________
            ! Aca se encuentra en cual subintervalo de la curva cae el x dado
            ! metodo eficiente para curva con abscisas equiespaciadas
            i_pre = CEILING( (x-x_min) / x_del )
            x1 = x_min + DFLOAT(i_pre-1)*x_del
            x2 = x1 + x_del
            y1 = yArray(i_pre)
            y2 = yArray(i_pre+1)
            ! Ahora se interpola el valor correspondiente de tension
            ! linear interpolation within the sub-interval
            slope = ( y2 - y1 ) / ( x2 - x1 )
            y = y1 + slope * (x - x1)
            ! __________________________________________________________________
        END IF
        ! ______________________________________________________________________

        ! ----------------------------------------------------------------------
    END FUNCTION ComputeFromCurve
    ! ==========================================================================


    ! ==========================================================================
    FUNCTION IntegCumTrapz(nn, xx, yy) RESULT(yy_acum)
        ! ----------------------------------------------------------------------
        ! Integral Cumulative Trapezoidal
        ! Regla de integracion trapezoidal para una curva discreta
        !
        ! nn: numero de puntos de la curva de entrada
        ! xx: valores de abscisas
        ! yy: valores de ordenadas
        ! yy_acum: valores de la integral a medida que se acumula
        ! ----------------------------------------------------------------------
        IMPLICIT NONE
        ! ----------------------------------------------------------------------
        INTEGER, INTENT(IN)  :: nn
        REAL(8), INTENT(IN)  :: xx(3), yy(nn)
        ! ----------------------------------------------------------------------
        REAL(8) :: yy_acum(nn)
        ! ----------------------------------------------------------------------
        INTEGER :: ii
        REAL(8) :: y_medio, delta_x
        ! ----------------------------------------------------------------------

        ! ______________________________________________________________________
        yy_acum(1) = 0.0d0
        delta_x = xx(3)
        DO ii=2,nn
            y_medio = 0.5d0 * ( yy(ii-1) + yy(ii) )
            yy_acum(ii) = yy_acum(ii-1) + y_medio*delta_x
        END DO
        ! ______________________________________________________________________

        ! ----------------------------------------------------------------------
    END FUNCTION IntegCumTrapz
    ! ==========================================================================


    ! ==========================================================================
    FUNCTION ProductoTensorial_nDim2(x, y) RESULT(outer)
        ! ----------------------------------------------------------------------
        ! Como el nombre lo indica, calcula el producto tensorial
        ! de dos vectores de dimension 2
        ! ----------------------------------------------------------------------
        IMPLICIT NONE
        ! ----------------------------------------------------------------------
        ! Parameters
        INTEGER, PARAMETER :: nDim = 2
        ! ----------------------------------------------------------------------
        ! Arguments
        REAL(8), INTENT(IN) :: x(nDim), y(nDim) !vectors of size nDim
        ! ----------------------------------------------------------------------
        ! Result
        REAL(8) :: outer(nDim,nDim)
        ! ----------------------------------------------------------------------
        ! Locals
        INTEGER :: i,j
        ! ----------------------------------------------------------------------

        ! ______________________________________________________________________
        DO i=1,nDim
            DO j=1,nDim
                outer(i,j) = x(i)*y(j)
            END DO
        END DO
        ! ______________________________________________________________________

        ! ----------------------------------------------------------------------
    END FUNCTION ProductoTensorial_nDim2
    ! =========================================================================


    ! ==========================================================================
    FUNCTION DeltaArray(n,i) RESULT(d)
        ! ----------------------------------------------------------------------
        ! Dado una dimemsion n y una posicion i
        ! calcula el array (0, 0, 0, ..., 1, ...,  0,  0)
        ! de posiciones:   (1, 2, 3, ..., i, ..., n-1, n)
        ! ----------------------------------------------------------------------
        IMPLICIT NONE
        ! ----------------------------------------------------------------------
        INTEGER, INTENT(IN) :: n,i
        REAL(8) :: d(n)
        ! ----------------------------------------------------------------------

        ! ______________________________________________________________________
        d=0.0d0
        d(i) = 1.0d0
        ! ______________________________________________________________________

        ! ----------------------------------------------------------------------
    END FUNCTION DeltaArray
    ! ==========================================================================

    ! ==========================================================================
    FUNCTION DiferenciaRelativa(x,y) RESULT(z)
        ! ----------------------------------------------------------------------
        ! Dado una dimemsion n y una posicion i
        ! calcula el array (0, 0, 0, ..., 1, ...,  0,  0)
        ! de posiciones:   (1, 2, 3, ..., i, ..., n-1, n)
        ! ----------------------------------------------------------------------
        IMPLICIT NONE
        ! ----------------------------------------------------------------------
        REAL(8), INTENT(IN) :: x,y
        REAL(8) :: z
        ! ----------------------------------------------------------------------

        ! ______________________________________________________________________
        IF (y<EPSILON(y)) THEN
            Write(*,*) "Error, intentando calcular una diferencia relativa sobre valor cero"
            STOP
        ELSE
            z = DABS(x-y)/y
        END IF
        ! ______________________________________________________________________

        ! ----------------------------------------------------------------------
    END FUNCTION DiferenciaRelativa
    ! ==========================================================================


END MODULE mod_algebra
! ==============================================================================
