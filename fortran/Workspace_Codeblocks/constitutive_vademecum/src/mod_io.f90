! ==============================================================================
MODULE mod_io

    ! --------------------------------------------------------------------------
    IMPLICIT NONE
    PRIVATE ! NOTE_avoid_public_variables_if_possible
    ! --------------------------------------------------------------------------
    PUBLIC :: Leer_Configuracion             ! TODO_optional_brief_description
    PUBLIC :: Escribir_Curva
    ! --------------------------------------------------------------------------

! ==============================================================================
CONTAINS
! ==============================================================================


    ! ==============================================================================
    SUBROUTINE Leer_Configuracion(s_configFile, &
                                  i_npLambda, rv_Lambda, &
                                  i_nParam, tv_Param, &
                                  s_dataset)
        use mod_strings
        USE class_parametro
        implicit none
        ! Parameters
        CHARACTER(LEN=120), INTENT(IN)  :: s_configFile
        INTEGER, INTENT(OUT) :: i_npLambda
        REAL(8), INTENT(OUT) :: rv_Lambda(3)
        INTEGER, INTENT(OUT) :: i_nParam
        TYPE(Parametro), ALLOCATABLE, INTENT(OUT) :: tv_Param(:)
        CHARACTER(LEN=120), INTENT(OUT)  :: s_dataset
        ! Current working directory
        CHARACTER(LEN=255) :: s_cwd
        ! Files
        INTEGER             :: i_fid
        ! Error check
        INTEGER             :: i_status
        INTEGER             :: i_input
        ! Auxiliares
        TYPE(Parametro) :: t_Lambda
        REAL(8) :: r_Size
        INTEGER :: j_param


        CALL GETCWD(s_cwd)
        PRINT *, "Hello! Working in directory: ", s_cwd
        PRINT *, "Reading from file: ", s_configFile

        ! leer parametros del vademecum (lambdas y parametros constitutivos)
        i_fid=11
        OPEN(UNIT=i_fid, FILE=TRIM(s_configFile), STATUS="OLD")
        ! leer nombre del archivo de salida nc
        i_status = FindStringInFile("* OUTPUT FILE", i_fid, .TRUE.)
        READ(i_fid,"(1A120)") s_dataset
        ! leer parametros para el vademecum constitutivo
        i_status = FindStringInFile("* BUNDLE CONSTITUTIVE PARAMETERS", i_fid, .TRUE.)
        ! leer lambdas
        t_Lambda = get_Parametro(i_fid)
        i_npLambda = t_Lambda%np
        rv_Lambda = t_Lambda%vals
        ! leer el numero de parametros constitutivos
        i_nParam = get_Integer(i_fid)
        ALLOCATE( tv_Param(i_nParam) )
        ! leer parametros constitutivos
        DO j_param=1,i_nParam
            tv_Param(j_param) = get_Parametro(i_fid)
        END DO
        CLOSE(UNIT=i_fid)

        ! Escribo lo que lei
        WRITE(*,"(1A51)") "Creating VADEMECUM for Bundle Constitutuve Response"
        WRITE(*,*)
        WRITE(*,"(1A8, 1A2, 1I8, 3f20.4)") "lambdas", ": ", i_npLambda, rv_Lambda
        WRITE(*,*)
        WRITE(*,"(1A9, 1A5, 3A20)") "Parametro", "np", "min", "max", "delta"
        WRITE(*,*)
        DO j_param=1,i_nParam
            WRITE(*,"(1A8, 1A2, 1I4, 3e20.4)") tv_Param(j_param)%nombre, ": ", tv_Param(j_param)%np,  tv_Param(j_param)%vals
        END DO

        r_Size = DFLOAT( PRODUCT(tv_Param%np) * i_npLambda ) * 8.0d-9

        ! Pregunto si quiero seguir adelante
        WRITE(*,*) "Expected size: ", r_Size, "GB"
        WRITE(*,*) "Continue? 1=yes"
        READ(*,*) i_input
        IF ( i_input .NE. 1 ) STOP

    END SUBROUTINE Leer_Configuracion
    ! ==============================================================================


    ! ==============================================================================
    SUBROUTINE Escribir_Curva(s_file, i_npLam, rv_Lam, rv_Ten)
        IMPLICIT NONE
        !
        CHARACTER(LEN=120), INTENT(IN) :: s_file
        INTEGER, INTENT(IN) :: i_npLam
        REAL(8), INTENT(IN) :: rv_Lam(i_npLam), rv_Ten(i_npLam)
        !
        INTEGER :: i_file
        INTEGER :: j_point
        REAL(8) :: r_lam
        !

        OPEN(FILE=TRIM(s_file), UNIT=i_file, STATUS="REPLACE")

        DO j_point=1, i_npLam
            r_lam = rv_Lam(1) + DFLOAT(j_point-1)*rv_Lam(3)
            WRITE(i_file,"(2E20.4)") r_lam, rv_Ten(j_point)
        END DO

        CLOSE(i_file)

    END SUBROUTINE
    ! ==============================================================================









    ! ==============================================================================
    ! AUXILIARES
    ! ==============================================================================

    ! ==============================================================================
    FUNCTION get_Integer (i_file) RESULT(i_value)
        ! lee un parametro del archivo de configuracion dado en i_file
        ! el formato del parametro es, en una linea: <nombre> = <min> <max> <npuntos>
        ! se leen los valores a una variable de tipo "Parametro"
        !
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: i_file
        INTEGER             :: i_value
        CHARACTER(LEN=120)  :: s_line
        INTEGER             :: i_sep
        !

        ! leo una linea
        READ(i_file, "(1A120)") s_line
        ! identifico donde esta el igual
        i_sep = INDEX(s_line,"=")
        ! leo el valor entero despues del igual
        READ(s_line(i_sep+1:), *) i_value

    END FUNCTION get_Integer
    ! ==============================================================================

    ! ==============================================================================
    FUNCTION get_Parametro (i_file) RESULT(t_parameter)
        ! lee un parametro del archivo de configuracion dado en i_file
        ! el formato del parametro es, en una linea: <nombre> = <min> <max> <npuntos>
        ! se leen los valores a una variable de tipo "Parametro"
        USE class_parametro
        !
        IMPLICIT NONE
        INTEGER, INTENT(IN) :: i_file
        TYPE(Parametro) :: t_parameter
        CHARACTER(LEN=120)  :: s_line
        INTEGER             :: i_sep
        !

        ! leo una linea
        READ(i_file, "(1A120)") s_line
        ! identifico donde esta el igual
        i_sep = INDEX(s_line,"=")
        ! leo el nombre del parametro antes del igual
        READ(s_line(:i_sep-1), "(1A10)") t_parameter%nombre
        ! leo los valores despues del igual
        READ(s_line(i_sep+1:), *) t_parameter%vals
        ! asigno el numero de puntos
        t_parameter%np = NINT(t_parameter%vals(3))
        ! calculo el delta entre valores segun los datos anteriores
        t_parameter%vals(3) = Calcular_Delta(t_parameter%vals(1), t_parameter%vals(2), t_parameter%np)
        ! recalculo los maximos ya que son redundantes con los deltas
        t_parameter%vals(2) = Calcular_Maximo(t_parameter%vals(1), t_parameter%vals(3), t_parameter%np)

    END FUNCTION get_Parametro
    ! ==============================================================================

    ! ==============================================================================
    FUNCTION Calcular_Delta(r_Min, r_Max, i_np) RESULT(r_Delta)
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: r_Min, r_Max
        INTEGER, INTENT(IN) :: i_np
        REAL(8) :: r_Delta

        IF ( i_np == 1 ) THEN
            r_Delta = 0.0d0
        ELSE
            r_Delta = ( r_Max-r_Min ) / DFLOAT( i_np-1 )
        END IF

    END FUNCTION Calcular_Delta
    ! ==============================================================================

    ! ==============================================================================
    FUNCTION Calcular_Maximo(r_Min, r_Del, i_np) RESULT(r_Maximo)
        IMPLICIT NONE
        REAL(8), INTENT(IN) :: r_Min, r_Del
        INTEGER, INTENT(IN) :: i_np
        REAL(8) :: r_Maximo

        r_Maximo = r_Min + DFLOAT(i_np-1)*r_Del

    END FUNCTION Calcular_Maximo
    ! ==============================================================================


END MODULE mod_io
! ==============================================================================
