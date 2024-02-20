
program hello
    USE netCDF
    implicit none
    ! ---
    ! dataset ID
    INTEGER :: i_fileID
    ! dimensiones
    INTEGER :: i_dim1_val, i_dim1_id
    INTEGER :: i_dim2_val, i_dim2_id
    INTEGER :: i_dim3_val, i_dim3_id
    INTEGER :: i_dimensions_ids(3)
    ! variables
    INTEGER                 :: i_var1_size1, i_var1_size2, i_var1_id
    REAL(8), ALLOCATABLE    :: r_var1_vals(:,:)
    INTEGER                 :: i_var2_size1, i_var2_size2, i_var2_id
    REAL(8), ALLOCATABLE    :: r_var2_vals(:,:)
    ! errores
    INTEGER             :: i_error
    CHARACTER(len=120)  :: c_error
    ! iteradores
    INTEGER :: i, j
    ! ---

    print *, "Hello World!"

    ! ---
    ! CREAR DATASET
    i_error = nf90_create("test.nc", nf90_clobber, i_fileID)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
    ! ---

    ! ---
    ! DEFINIR DIMENSIONES
    ! asignar valores
    i_dim1_val = 5
    i_dim2_val = 3
    i_dim3_val = 2
    ! definir dim1
    i_error = nf90_def_dim(i_fileID, "dim1", i_dim1_val, i_dim1_id)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
    ! definir dim2
    i_error = nf90_def_dim(i_fileID, "dim2", i_dim2_val, i_dim2_id)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
    ! definir dim3
    i_error = nf90_def_dim(i_fileID, "dim3", i_dim3_val, i_dim3_id)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
    ! ---

    ! ---
    ! CREAR VARIABLE
    ! crear variable
    i_dimensions_ids = (/ i_dim1_id, i_dim2_id, i_dim3_id /)
    i_error = nf90_def_var(i_fileID, 'var1', nf90_double, i_dimensions_ids, i_var1_id)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
    ! ---

    ! ---
    ! TERMINAR ETAPA DE DEFINICION
    i_error = nf90_enddef(i_fileID)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
    ! ---

    ! ---
    ! ETAPA DE ESCRITURA - LECTURA
    ! ---


    ! ---
    ! ESCRIBIR VARIABLE
    ! primero armo un array local
    ! allocate array en memoria
    i_var1_size1 = 2
    i_var1_size2 = 3
    ALLOCATE( r_var1_vals(i_var1_size1, i_var1_size2) )
    ! asignar valores al array en memoria
    DO i=1,i_var1_size1
        DO j=1,i_var1_size2
            r_var1_vals(i,j) = (i-1)*i_var1_size2 + j
        END DO
    END DO
    ! escribir en pantalla para chequear
    DO i=1,i_var1_size1
        DO j=1,i_var1_size2
            WRITE(*,"(f8.2)",ADVANCE="NO") r_var1_vals(i,j)
        END DO
        WRITE(*,*)
    END DO
    WRITE(*,*)
    ! asignar valores
    i_error = nf90_put_var(i_fileID, i_var1_id, r_var1_vals, start=[1,1,1], stride=[2,2])
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
    ! ---

    ! ---
    ! CERRAR DATASET
    i_error = nf90_close(i_fileID)
    ! ---

    ! ---
    ! LEER VARIABLE DE DATASET
    ! abrir dataset para lectura
    i_error = nf90_open("test.nc", nf90_nowrite, i_fileID)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
    ! adjudicar memoria local para leer variable
    i_var2_size1 = i_var1_size1
    i_var2_size2 = i_var1_size2
    ALLOCATE( r_var2_vals(i_var2_size1, i_var2_size2) )
    ! obtener id de la variable var1
    i_error = nf90_inq_varid(i_fileID, "var1", i_var2_id)
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
    ! obtener valores de la variable var1
    i_error = nf90_get_var(i_fileID, i_var2_id, r_var2_vals, start=[1,1,1], stride=[2,2])
    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
    ! escribir en pantalla para chequear
    DO i=1,i_var2_size1
        DO j=1,i_var2_size2
            WRITE(*,"(f8.2)",ADVANCE="NO") r_var2_vals(i,j)
        END DO
        WRITE(*,*)
    END DO
    ! ---


end program
