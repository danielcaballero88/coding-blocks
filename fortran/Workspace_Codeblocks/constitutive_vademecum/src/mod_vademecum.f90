! ==============================================================================
MODULE mod_vademecum

    ! --------------------------------------------------------------------------
    USE netCDF
    IMPLICIT NONE
    PRIVATE ! NOTE_avoid_public_variables_if_possible
    ! --------------------------------------------------------------------------
    PUBLIC :: Write_Vademecum             ! TODO_optional_brief_description
    ! --------------------------------------------------------------------------


! ==============================================================================
CONTAINS
! ==============================================================================

    ! ==========================================================================
    SUBROUTINE Write_Vademecum &
        (s_File, &
        i_nLams, rv_Lams, &
        i_nParam, tv_Param, &
        EcuacionConstitutiva)
        ! ----------------------------------------------------------------------
        ! DESCRIPTION:
        ! Escribe un vademecum para una ley constitutiva 1D
        !
        ! Los arrays se tratan de manera compacta (formato equiespaciado):
        ! v = (vmin, vmax, vdel) = vmin, vmin+vdel, vmin+2*vdel, ..., vmin+(i_nv-1)*vdel
        ! donde v es un array cualquiera, equiespaciado, con i_nv cantidad de valores
        ! vmax es superfluo pero ya elegi asi el formato
        ! ----------------------------------------------------------------------
        USE class_parametro
        USE netCDF
        USE mod_bundle
        IMPLICIT NONE
        ! ----------------------------------------------------------------------
        CHARACTER(LEN=120), INTENT(IN)  :: s_File                  ! Nombre del archivo .nc del vademecum
        INTEGER, INTENT(IN)             :: i_nLams                 ! Cantidad de valores de deformacion
        REAL(8), INTENT(IN)             :: rv_Lams(3)              ! Array con valores de deformacion
        INTEGER, INTENT(IN)             :: i_nParam                ! Cantidad de parametros de la ley constitutiva
        TYPE(Parametro), INTENT(IN)     :: tv_Param(i_nParam)
        EXTERNAL :: EcuacionConstitutiva
        INTERFACE
            SUBROUTINE EcuacionConstitutiva(i__nLams, rv__Lams, i__nConPar, rv__ConPar, rv__Tensiones)
                        ! entrada
                        INTEGER, INTENT(IN) :: i__nLams    ! numero de puntos de la curva constitutiva
                        REAL(8), INTENT(IN) :: rv__Lams(3) ! (lambda_min, lambda_max, lambda_delta)
                        INTEGER, INTENT(IN) :: i__nConPar     ! numero de parametros constitutivos
                        REAL(8), INTENT(IN) :: rv__ConPar(i__nConPar) ! parametros constitutivos
                        ! salida
                        REAL(8), INTENT(OUT) :: rv__Tensiones(i__nLams)
            END SUBROUTINE
        END INTERFACE
        ! ----------------------------------------------------------------------
        ! netCDF
        INTEGER :: i_error
        INTEGER :: i_file
        INTEGER :: dim_lam, dim_params(i_nParam)
        CHARACTER(LEN=14) :: s_dim_name
        INTEGER :: var_Tens
        INTEGER :: i_ncomb
        INTEGER :: j_param, j_comb, jv_ind(i_nParam)
        REAL(8) :: rv_Param(i_nParam)
        REAL(8) :: rv_Tens(i_nLams)
        ! ----------------------------------------------------------------------
        ! auxiliar
        CHARACTER(LEN=120) :: s_formato
        ! ----------------------------------------------------------------------

        ! ______________________________________________________________________
        !
        ! Crear archivo del vademecum
        i_error = nf90_create(s_File, nf90_clobber, i_file)
        if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
        ! ----------
        ! Definir dimensiones
        ! una para la deformacion 1d lambda
        i_error = nf90_def_dim(i_file, "dim_lambda", i_nLams, dim_lam)
        if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
        ! y una para cada parametro
        DO j_param=1,i_nParam
            WRITE(s_dim_name,"(1A4,1A10)") "dim_", tv_Param(j_param)%nombre
            i_error = nf90_def_dim(i_file, trim(s_dim_name), tv_Param(j_param)%np, dim_params(j_param))
            if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
        END DO
        ! ----------
        ! Definir variable
        i_error = nf90_def_var(i_file, "var_tensiones", nf90_double, [dim_lam, dim_params], var_Tens)
        if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
        ! ----------
        ! Terminar etapa de definicion
        i_error = nf90_enddef(i_file)
        if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
        ! ----------
        ! Calcular tensiones por pedazos para guardar en el vademecum

        ! Bucle anidado desdoblegado en uno solo
        i_ncomb = PRODUCT(tv_Param%np)
        WRITE(*,*) " Inciando a calcular, cantidad de pedazos: ", i_ncomb
        DO j_comb=1,i_ncomb
            ! ==========
            ! Calculo indices de bucles anidados
            IF (j_comb==1) THEN
                jv_ind = 1
            ELSE
                CALL contador(i_nParam, tv_Param%np, jv_ind)
            END IF
            ! ==========
            ! Calculo valores de parametros en esos indices
            DO j_param=1,i_nParam
                IF ( tv_Param(j_param)%np == 1) THEN
                    rv_Param(j_param) = tv_Param(j_param)%vals(1)
                ELSE
                    rv_Param(j_param) = tv_Param(j_param)%vals(1) + DFLOAT(jv_ind(j_param)-1)*tv_Param(j_param)%vals(3)
                END IF
            END DO
            ! ==========
            ! Calcular pedazo
            WRITE(s_formato, "(1A6,1I2,1A4)") "(1A20,",i_nParam,"I4)"
            WRITE(*,s_formato) "Calculando pedazo: ", jv_ind
            CALL EcuacionConstitutiva(i_nLams, rv_Lams, &
                                           i_nParam, rv_Param, &
                                           rv_Tens)
            ! ==========
            ! Guardar pedazo en el vademecum
            WRITE(*,*) "Almacenando pedazo: "
            i_error = nf90_put_var(i_file, var_Tens, rv_Tens, start=[1,jv_ind])
            if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
            ! ==========

        END DO

        ! ==========
        ! Cerrar dataset
        i_error = nf90_close(i_file)
        if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
        ! ==========

        ! ----------
        ! ______________________________________________________________________

        ! ----------------------------------------------------------------------
    END SUBROUTINE Write_Vademecum
    ! ==========================================================================

    ! ==========================================================================
    SUBROUTINE contador(i_numind, i_v_maxind, i_v_comb)
        ! subrutina para desdoblar bucles anidados en uno solo
        ! es util cuando no se sabe la cantidad de bucles a anidar de antemano
        ! como en el caso que hay que recorrer los elementos de un array
        ! multidimensional allocatable
        ! es util imaginarse un odometro
        IMPLICIT NONE
        !
        INTEGER, INTENT(IN) :: i_numind ! numero de indices a simular
        INTEGER, INTENT(IN) :: i_v_maxind(i_numind) ! numero maximo de valor por indice
        INTEGER, INTENT(INOUT) :: i_v_comb(i_numind)
        !
        LOGICAL :: b_v_reset(i_numind)
        INTEGER :: i_old, i_new, i_max, i_pre
        INTEGER :: k
        !

        ! inicializo
        b_v_reset = .FALSE.

        ! chequeo el cambio del primer indice primero
        i_old = i_v_comb(1)
        i_new = i_old+1 ! candidato
        i_max = i_v_maxind(1)
        ! calculo el nuevo valor segun las opciones
        IF (i_new > i_max)  THEN
            ! si supere el maximo debo resetear el indice
            i_new = 1
            b_v_reset(1) = .TRUE.
        END IF
        i_v_comb(1) = i_new
        ! ahora calculo los siguientes indices
        DO k=2,i_numind
            ! guardo valores auxiliares utiles
            i_old = i_v_comb(k)
            i_new = i_old + 1 ! candidato
            i_max = i_v_maxind(k)
            i_pre = i_v_comb(k-1)
            ! chequeo si el indice previo (contiguo a la izquierda) se ha reseteado
            IF ( b_v_reset(k-1) ) THEN
                ! si el indice previo ha reseteado debo cambiar
                IF (i_new > i_max)  THEN
                    ! si supere el maximo debo resetear
                    i_new = 1
                    b_v_reset(k) = .TRUE.
                END IF
            ELSE
                ! si el indice previo no ha reseteado debo seguir igual
                i_new = i_old
            END IF
            i_v_comb(k) = i_new
        END DO

    END SUBROUTINE contador
    ! ==========================================================================

END MODULE mod_vademecum
! ==============================================================================
