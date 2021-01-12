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
MODULE mod_bundle

    ! --------------------------------------------------------------------------
    USE mod_algebra
    ! --------------------------------------------------------------------------
    IMPLICIT NONE
    PRIVATE ! NOTE_avoid_public_variables_if_possible
    ! --------------------------------------------------------------------------
    PUBLIC :: Get_CurvaConstitutiva_Haz ! Calcula en arrays la curva constitutiva del haz de manera discreta
    ! --------------------------------------------------------------------------

! ==============================================================================
CONTAINS
! ==============================================================================



    ! ==========================================================================
    SUBROUTINE Get_CurvaConstitutiva_Haz &
        (i_nlambdas, rv_lambdas, &
        i_nconpar, rv_conpar, &
        rv_tensiones)
        ! ----------------------------------------------------------------------
        ! Calcula la curva constitutiva para un haz de fibras
        ! cada fibra con respuesta bilineal (ver paper Caballero et al)
        ! los valores de lambda estan equiespaciados
        ! en miras de tener mejor rendimiento al utilizarla luego
        ! el vector de deformaciones se da en 3 valores: (lam_min, lam_max, lam_del)
        ! ademas se acompania con el numero de valores de lambda
        ! (en rigor i_nlambdas y lam_max son redundantes)
        ! ----------------------------------------------------------------------
        IMPLICIT NONE
        ! ----------------------------------------------------------------------
        ! entrada
        INTEGER, INTENT(IN) :: i_nlambdas    ! numero de puntos de la curva constitutiva
        REAL(8), INTENT(IN) :: rv_lambdas(3) ! (lambda_min, lambda_max, lambda_delta)
        INTEGER, INTENT(IN) :: i_nconpar     ! numero de parametros constitutivos
        REAL(8), INTENT(IN) :: rv_conpar(i_nconpar) ! parametros constitutivos
        ! salida
        REAL(8), INTENT(OUT) :: rv_tensiones(i_nlambdas)
        ! ----------------------------------------------------------------------
        ! locales
        ! auxiliares
        REAL(8) :: lambda_min, lambda_del
        REAL(8) :: Et, Eb, lamr0, lamrS, lamr_min, lamr_max
        INTEGER :: nPuntos_cdnt ! numero de puntos de la curva distribucion normal truncada
        ! curva distribucion normal truncada y normalizada
        REAL(8), ALLOCATABLE :: lamr(:), pdf1(:), cdf1(:)
        REAL(8)              :: lamr_i
        ! integrandos e integrados de la curva de distribucion:
        REAL(8), ALLOCATABLE :: pdf2(:), cdf2(:), pdf3(:), cdf3(:)
        ! auxiliares
        INTEGER :: ii
        REAL(8) :: lambda_i, C1, C2, C3, tension_i
        ! ----------------------------------------------------------------------

        ! ______________________________________________________________________
        lambda_min = rv_lambdas(1)
        lambda_del = rv_lambdas(3)
        Et = rv_conpar(1)
        Eb = rv_conpar(2)
        lamr0 = rv_conpar(3)
        lamrS = rv_conpar(4)
        lamr_min = rv_conpar(5)
        lamr_max = rv_conpar(6)
        nPuntos_cdnt = NINT(rv_conpar(7))
        ! ______________________________________________________________________
        ALLOCATE( lamr(3) )
        ALLOCATE( pdf1(nPuntos_cdnt) )
        ALLOCATE( cdf1(nPuntos_cdnt) )
        ALLOCATE( pdf2(nPuntos_cdnt) )
        ALLOCATE( cdf2(nPuntos_cdnt) )
        ALLOCATE( pdf3(nPuntos_cdnt) )
        ALLOCATE( cdf3(nPuntos_cdnt) )
        ! ______________________________________________________________________
        CALL Calcular_CurvaDistribucionNormalTruncada(lamr0, lamrS, nPuntos_cdnt, lamr_min, lamr_max, lamr, pdf1, cdf1)
        ! ______________________________________________________________________
        DO ii=1,nPuntos_cdnt
            lamr_i = lamr(1) + DFLOAT(ii-1)*lamr(3)
            pdf2(ii) = pdf1(ii) * lamr_i
            pdf3(ii) = pdf1(ii) / lamr_i
        END DO
        cdf2 = IntegCumTrapz(nPuntos_cdnt, lamr, pdf2)
        cdf3 = IntegCumTrapz(nPuntos_cdnt, lamr, pdf3)
        DO ii=1,i_nlambdas
            lambda_i = lambda_min + DFLOAT(ii-1)*lambda_del
            C1 = ComputeFromCurve(lambda_i, nPuntos_cdnt, lamr, cdf1, .TRUE.)
            C2 = ComputeFromCurve(lambda_i, nPuntos_cdnt, lamr, cdf2, .TRUE.)
            C3 = ComputeFromCurve(lambda_i, nPuntos_cdnt, lamr, cdf3, .TRUE.)
            tension_i = Eb*(lambda_i-1.0d0)*(1.0d0 - C1) + Eb*(C2-C1) + Et*(lambda_i*C3-C1)
            rv_tensiones(ii) = tension_i
        END DO
        ! ______________________________________________________________________

        ! ----------------------------------------------------------------------
    END SUBROUTINE Get_CurvaConstitutiva_Haz
    ! ==========================================================================

    ! ==========================================================================
    ! ==========================================================================
    ! METODOS AUXILIARES
    ! ==========================================================================
    ! ==========================================================================


    ! ==========================================================================
    SUBROUTINE Calcular_CurvaDistribucionNormalTruncada(mu, sigma, nPuntos, x_min, x_max, xx, pdf, cdf)
        ! ----------------------------------------------------------------------
        ! Devuelve una curva de distribucion normal
        ! la curva esta truncada entre "x_min" y "x_max"
        ! y renormalizada para que el area bajo la curva sea 1
        !
        ! mu = valor medio
        ! sigma = desviacion estandar
        ! npuntos = numero de puntos de los arrays de la curva
        ! xx = array de valores de abscisas
        ! pdf = array de distribucion normal
        ! cdf = array de distribucion normal acumulada
        ! ----------------------------------------------------------------------
        IMPLICIT NONE
        ! ----------------------------------------------------------------------
        REAL(8), PARAMETER :: PI = 4.0d0*DATAN(1.0d0)
        ! ----------------------------------------------------------------------
        REAL(8), INTENT(IN)  :: mu, sigma
        INTEGER, INTENT(IN)  :: nPuntos
        REAL(8), INTENT(IN)  :: x_min, x_max ! limites donde se trunca la distribucion normal
        REAL(8), INTENT(OUT) :: xx(3), pdf(nPuntos), cdf(nPuntos)
        ! ----------------------------------------------------------------------
        REAL(8) :: dx, x
        INTEGER :: ii
        REAL(8) :: aux
        ! ----------------------------------------------------------------------

        ! ______________________________________________________________________
        ! calculo el valor de espaciado entre puntos
        dx = (x_max - x_min ) / DFLOAT(nPuntos-1)
        ! ______________________________________________________________________
        ! ensamblo el array de abscisas (xx) y el de ordenadas (pdf)
        DO ii=1,nPuntos
            x = x_min + DFLOAT(ii-1)*dx
            pdf(ii) = (1.0d0 / DSQRT(2.0d0*PI*sigma**2)) * DEXP( - (x-mu)**2 / (2.0d0*sigma**2) )
        END DO
        xx = [x_min, x, dx]
        ! ______________________________________________________________________
        ! computo el array de distribucion acumulada sin normalizar (cdf)
        cdf = IntegCumTrapz(nPuntos, xx, pdf)
        ! normalizo las curvas
        aux = cdf(nPuntos)
        DO ii=1,nPuntos
            pdf(ii) = pdf(ii) / aux
            cdf(ii) = cdf(ii) / aux
        END DO
        ! ______________________________________________________________________

        ! ----------------------------------------------------------------------
    END SUBROUTINE Calcular_CurvaDistribucionNormalTruncada
    ! ==========================================================================



END MODULE mod_bundle
! ==============================================================================
