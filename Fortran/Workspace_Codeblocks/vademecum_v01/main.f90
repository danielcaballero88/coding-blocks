
program hello
    USE mod_constitutive
    USE netCDF
    implicit none
    ! parametros
    REAL(8) :: rvi_p1(3), r_p1
    REAL(8) :: rvi_p2(3), r_p2
    REAL(8) :: rvi_p3(3), r_p3
    INTEGER :: len_p1, len_p2, len_p3
    REAL(8) :: rv_params_vals(3)
    ! deformacion
    REAL(8) :: rvi_C(3)
    INTEGER :: len_C
    INTEGER :: j_p1,j_p2,j_p3,j_C11,j_C22,j_C12
    REAL(8) :: r_C11, r_C22, r_C12
    REAL(8) :: rT_C(3), rT_S(3)
    ! netCDF variables
    INTEGER :: i_error
    CHARACTER(LEN=120) :: c_error
    INTEGER :: id_file
    INTEGER :: id_dim_S, id_dim_C11, id_dim_C22, id_dim_C12, id_dim_p1, id_dim_p2, id_dim_p3
    INTEGER :: ids_array(7)
    INTEGER :: id_S
    ! parte del vademecum
    REAL(8), ALLOCATABLE :: r_vad_chunk(:,:,:,:)


    ! ----------
    ! establecer arrays implicitos de deformacion
    rvi_C = [0.7d0, 1.3d0, 0.01d0]
    len_C = CEILING((rvi_C(2)-rvi_C(1))/rvi_C(3)) + 1
    ! establecer arrays implicitos de parametros
    rvi_p1 = [1.0d0, 10.0d0, 0.1d0]
    len_p1 = CEILING((rvi_p1(2)-rvi_p1(1))/rvi_p1(3)) + 1
    rvi_p2 = [1.0d0, 1.1d0, 0.1d0]
    len_p2 = CEILING((rvi_p2(2)-rvi_p2(1))/rvi_p2(3)) + 1
    rvi_p3 = [0.01d0, 0.1d0, 0.001d0]
    len_p3 = CEILING((rvi_p3(2)-rvi_p3(1))/rvi_p3(3)) + 1
    ! ----------

    ! ----------
    ! adjudico memoria para la seccion de vademecum a almacenar
    ALLOCATE( r_vad_chunk(3,len_C,len_C,len_C) )
    ! ----------

    print*, 3, len_C, len_C, len_C, len_p1, len_p2, len_p3
    print*, 3 * len_C * len_C * len_C * len_p1 * len_p2 * len_p3
    print*,

    ! ----------
    ! ----------
    ! ----------
    ! crear dataset
    i_error = nf90_create("vademecum.nc", nf90_clobber, id_file)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
    ! -
    ! definir dimensiones
    i_error = nf90_def_dim(id_file, "dim_S", 3, id_dim_S)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)

    i_error = nf90_def_dim(id_file, "dim_C11", len_C, id_dim_C11)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)

    i_error = nf90_def_dim(id_file, "dim_C22", len_C, id_dim_C22)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)

    i_error = nf90_def_dim(id_file, "dim_C12", len_C, id_dim_C12)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)

    i_error = nf90_def_dim(id_file, "dim_p1", len_p1, id_dim_p1)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)

    i_error = nf90_def_dim(id_file, "dim_p2", len_p2, id_dim_p2)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)

    i_error = nf90_def_dim(id_file, "dim_p3", len_p3, id_dim_p3)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
    ! -
    ! definir variable
    i_error = nf90_def_var(id_file, "S", nf90_double, [id_dim_S, id_dim_C11, id_dim_C22, id_dim_C12, id_dim_p1, id_dim_p2, id_dim_p3], id_S)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
    ! -
    ! terminar etapa de definicion
    i_error = nf90_enddef(id_file)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
    ! ----------
    ! ----------
    ! ----------


    ! ----------
    ! ----------
    ! LOOP para variar los parametros, para cada set de parametros voy a armar
    ! una parte del vademecum y lo voy a almacenar en el dataset
    DO j_p1=1,len_p1
        r_p1 = rvi_p1(1) + DFLOAT(j_p1-1)*rvi_p1(3)
        DO j_p2=1,len_p2
            r_p2 = rvi_p2(1) + DFLOAT(j_p2-1)*rvi_p2(3)
            DO j_p3=1,len_p3
                r_p3 = rvi_p3(1) + DFLOAT(j_p3-1)*rvi_p3(3)
                ! ----------
                ! ----------
                ! ----------
                ! ----------

                print*, "armando chunk: ", j_p1, j_p2, j_p3

                ! Aqui va el codigo para ir armando el vademecum de a partes
                ! ----------
                ! array con los parametros de la ecuacion constitutiva
                rv_params_vals = [r_p1, r_p2, r_p3]

                ! ----------
                ! ----------
                ! LOOP para variar las deformaciones y asi armar la parte del vademecum
                DO j_C11=1,len_C
                    r_C11 = rvi_C(1) + DFLOAT(j_C11-1)*rvi_C(3)
                    DO j_C22=1,len_C
                        r_C22 = rvi_C(1) + DFLOAT(j_C22-1)*rvi_C(3)
                        DO j_C12=1,len_C
                            r_C12 = rvi_C(1) + DFLOAT(j_C12-1)*rvi_C(3)
                            ! ----------
                            ! ----------
                            ! calculo la tension para el valor de deformacion y set de parametros
                            rT_C = [r_C11, r_C22, r_C12]
                            rT_S = linearSimple(rT_C, rv_params_vals)
                            r_vad_chunk(:,j_C11,j_C22,j_C12) = rT_S
                            ! ----------
                            ! ----------
                        END DO
                    END DO
                END DO
                ! ----------
                ! ----------

                print*, "almacenando chunk"

                ! ----------
                ! guardo en el dataset la parte del vademecum correspondiente
                i_error = nf90_put_var(id_file, id_S, r_vad_chunk, start=[1,1,1,1,j_p1,j_p2,j_p3])
                if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
                ! ----------

                ! ----------
                ! ----------
            END DO
        END DO
    END DO
    ! ----------
    ! ----------

    ! cerrar dataset
    i_error = nf90_close(id_file)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)

!    ! ----------
!    ! write to file in ASCII
!    OPEN(unit=11, file="vademecum.txt", STATUS="REPLACE")
!    DO i=1,i_Clen
!        DO j=1,i_Clen
!            DO k=1,i_Clen
!                WRITE(11,"(3f10.2)") r_vad1(:,i,j,k)
!            END DO
!        END DO
!    END DO
!    CLOSE(11)
!    ! ----------
!
!    ! ----------
!    ! write to file in netCDF
!    ! -
!
!    ! -
!    ! escribir valores
!    i_error = nf90_put_var(id_file, i_varS_id, r_vad1)
!    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
!    ! -
!    ! cerrar dataset
!    i_error = nf90_close(id_file)
!    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
!    ! -
!    ! chequeo leyendo un valor de tension
!    i_error = nf90_open("vademecum.nc", nf90_nowrite, id_file)
!    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
!
!    i_error = nf90_inq_varid(id_file, "S", i_varS_id)
!    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
!
!    i_error = nf90_get_var(id_file, i_varS_id, rT_S, start=[1,4,5,2], stride=[1,1])
!    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
!
!    print*, rT_S
!
!    i_error = nf90_close(id_file)
!    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
!    ! ----------

end program

