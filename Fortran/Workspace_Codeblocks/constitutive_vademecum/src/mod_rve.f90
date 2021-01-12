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
MODULE mod_rve

    ! --------------------------------------------------------------------------
    USE mod_algebra
    ! --------------------------------------------------------------------------
    IMPLICIT NONE
    PRIVATE ! NOTE_avoid_public_variables_if_possible
    ! --------------------------------------------------------------------------
    PUBLIC :: RVE_Respuesta
    ! --------------------------------------------------------------------------

! ==============================================================================
CONTAINS
! ==============================================================================


    ! ==========================================================================
    FUNCTION RVE_Respuesta(FF, nBundlesRVE, bundles0, CCh_np, CCh_lam, CCh_ten, volumeFraction, lr0) RESULT(fpkStress)
        ! ----------------------------------------------------------------------
        ! DESCRIPTION:
        ! TODO_describe_routine_purpose.
        ! ----------------------------------------------------------------------
        IMPLICIT NONE
        ! ----------------------------------------------------------------------
        INTEGER, PARAMETER      :: nDim = 2
        ! ----------------------------------------------------------------------
        REAL(8), INTENT(IN)     :: FF(nDim,nDim)                    ! Deformation gradient
        INTEGER, INTENT(IN)     :: nBundlesRVE                      ! Number of members that compose the RVE
        REAL(8), INTENT(IN)     :: bundles0(nDim,nBundlesRVE)       ! Array with the initial unity vectors of the fibers
        INTEGER, INTENT(IN)     :: CCh_np                           ! numero de puntos de la curva constitutiva del haz
        REAL(8), INTENT(IN)     :: CCh_lam(3), CCh_ten(CCh_np) ! arrays de la curva constitutiva del haz (lambdas y tensiones)
        REAL(8), INTENT(IN)     :: volumeFraction                   ! Volume fraction of the macroscopic material
        REAL(8), INTENT(IN)     :: lr0                              ! valor medio de la distribucion de reclutamiento
        ! ----------------------------------------------------------------------
        REAL(8)                 :: fpkStress(nDim,nDim)             ! First Piola Kirchhoff Stress Tensor (average of the fibers)
        ! ----------------------------------------------------------------------
        REAL(8) :: bundle(nDim) ! member deformed vector
        REAL(8) :: b0xb0(nDim,nDim) ! outer product of a bundle direction with itself
        REAL(8) :: lambda
        INTEGER :: i ! counter
        REAL(8) :: bundleStress
        ! ----------------------------------------------------------------------

        ! ______________________________________________________________________
        ! RVE Stress as a sum over the RVE members
        fpkStress = 0.0d0
        DO i=1,nBundlesRVE
            ! quasi-affine deformation of each member
            bundle(1) = FF(1,1)*bundles0(1,i) + FF(1,2)*bundles0(2,i)
            bundle(2) = FF(2,1)*bundles0(1,i) + FF(2,2)*bundles0(2,i)
            lambda = DSQRT(SUM(bundle*bundle))
            ! Stress excerted by the member
            bundleStress = ComputeFromCurve(lambda, CCh_np, CCh_lam, CCh_ten)
            ! Sum bundle stress to the RVE stress
            b0xb0 = ProductoTensorial_nDim2(bundles0(1,i), bundles0(1,i))
            fpkStress = fpkStress + bundleStress/lambda * b0xb0
        END DO
        ! Now complete the computation of RVE stress (products common to all members)
        fpkStress = ( (volumeFraction/lr0) / nBundlesRVE ) * MATMUL(FF,fpkStress)
        ! ______________________________________________________________________

        ! ----------------------------------------------------------------------
    END FUNCTION RVE_Respuesta
    ! ==========================================================================


END MODULE mod_rve
! ==============================================================================
