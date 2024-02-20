MODULE test_module
    IMPLICIT NONE

    PRIVATE

    PUBLIC :: EyeMatrix
    PUBLIC :: VdotV_1, VdotV_2, VdotV_3
    PUBLIC :: TxT_1, TxT_2, TxT_3
    PUBLIC :: write_matrix, write_matrix4

CONTAINS

    !====================================================================================
    FUNCTION VdotV_1(v1, v2, nDim) RESULT (v3)
        !====================================================================================
        IMPLICIT NONE
        !====================================================================================
        ! Arguments
        INTEGER, INTENT(IN) :: nDim
        REAL(8), INTENT(IN) :: v1(nDim), v2(nDim)
        ! Result
        REAL(8) :: v3(nDim)
        ! Locals
        INTEGER :: i
        !====================================================================================

        !====================================================================================
        v3 = v1*v2
        !====================================================================================

    END FUNCTION
    !====================================================================================

    !====================================================================================
    FUNCTION VdotV_2(v1, v2, nDim) RESULT (v3)
        !====================================================================================
        IMPLICIT NONE
        !====================================================================================
        ! Arguments
        INTEGER, INTENT(IN) :: nDim
        REAL(8), INTENT(IN) :: v1(:), v2(:)
        ! Result
        REAL(8) :: v3(nDim)
        ! Locals
        INTEGER :: i
        !====================================================================================

        !====================================================================================
        v3 = v1*v2
        !====================================================================================

    END FUNCTION
    !====================================================================================

    !====================================================================================
    FUNCTION VdotV_3(v1, v2) RESULT (v3)
        !====================================================================================
        IMPLICIT NONE
        !====================================================================================
        ! Parameters
        INTEGER, PARAMETER :: nDimL = 100000
        ! Arguments
        REAL(8), INTENT(IN) :: v1(:), v2(:)
        ! Result
        REAL(8) :: v3(nDimL)
        ! Locals
        INTEGER :: i
        !====================================================================================

        !====================================================================================
        v3 = v1*v2
        !====================================================================================

    END FUNCTION
    !====================================================================================

    !====================================================================================
    FUNCTION TxT_1(A, B, nDim) RESULT(C) ! Aplicacion de matrices C=AB
        !================================================================================
        IMPLICIT NONE
        ! Arguments
        INTEGER, INTENT(IN) :: nDim    !dimensions
        REAL(8), INTENT(IN) :: A(nDim,nDim), B(nDim,nDim)
        ! Result
        REAL(8) :: C(nDim,nDim)
        ! Counter
        INTEGER :: i,j,k
        !================================================================================
        C = 0.0d0
        DO i=1,nDim
            DO j=1,nDim
                DO k=1,nDim
                    C(i,j) =  C(i,j) + A(i,k)*B(k,j)
                ENDDO
            ENDDO
        ENDDO
        !================================================================================
    END FUNCTION
    !====================================================================================

    !====================================================================================
    FUNCTION TxT_2(A, B, nDim) RESULT(C) ! Aplicacion de matrices C=AB
        !================================================================================
        IMPLICIT NONE
        ! Arguments
        INTEGER, INTENT(IN) :: nDim    !dimensions
        REAL(8), INTENT(IN) :: A(:,:), B(:,:)
        ! Result
        REAL(8) :: C(nDim,nDim)
        ! Counter
        INTEGER :: i,j,k
        !================================================================================
        C = 0.0d0
        DO i=1,nDim
            DO j=1,nDim
                DO k=1,nDim
                    C(i,j) =  C(i,j) + A(i,k)*B(k,j)
                ENDDO
            ENDDO
        ENDDO
        !================================================================================
    END FUNCTION
    !====================================================================================

    !====================================================================================
    FUNCTION TxT_3(A, B, nDim2) RESULT(C) ! Aplicacion de matrices C=AB
        !================================================================================
        IMPLICIT NONE
        ! Arguments
        INTEGER, PARAMETER :: nDim=3    !dimensions
        INTEGER, INTENT(IN) :: nDim2
        REAL(8), INTENT(IN) :: A(nDim2,nDim2), B(nDim2,nDim2)
        ! Result
        REAL(8) :: C(nDim2,nDim2)
        ! Counter
        INTEGER :: i,j,k
        !================================================================================
        C = 0.0d0
        DO i=1,nDim2
            DO j=1,nDim2
                DO k=1,nDim2
                    C(i,j) =  C(i,j) + A(i,k)*B(k,j)
                ENDDO
            ENDDO
        ENDDO
        !================================================================================
    END FUNCTION
    !====================================================================================

    !====================================================================================
    FUNCTION EyeMatrix(nDim) RESULT(Eye)
        !================================================================================
        IMPLICIT NONE
        ! Arguments
        INTEGER, INTENT(IN) :: nDim
        ! Result
        REAL(8) :: Eye(nDim,nDim)
        ! Counter
        INTEGER :: i
        !================================================================================
        Eye = 0.0d0
        DO i=1,nDim
            Eye(i,i) = 1.0d0
        END DO
        !================================================================================
    END FUNCTION
    !====================================================================================

    SUBROUTINE write_matrix(matrix,n,m,name, format_type)
        IMPLICIT NONE
        !arguments
        INTEGER, INTENT(IN) :: n,m
        REAL(8), DIMENSION(n,m), INTENT(IN) :: matrix
        CHARACTER(*), INTENT(IN), OPTIONAL :: name, format_type
        !local variables
        CHARACTER(20) :: FORMAT
        CHARACTER(100) :: matname
        CHARACTER(10) :: fmt_type
        INTEGER :: i,j

        matname = "unnamed matrix"
        IF (PRESENT(name)) matname=name
        fmt_type = "long"
        IF (PRESENT(format_type)) fmt_type=format_type

        CALL format_selector(fmt_type,FORMAT,m)

        WRITE(*,*)      "WRITING ", matname
        DO i=1,n
            WRITE(*,FORMAT) (matrix(i,j),j=1,m)
        ENDDO
        WRITE(*,*)

    END SUBROUTINE write_matrix

    SUBROUTINE write_matrix4(matrix,n,m,o,p,name, format_type)
        IMPLICIT NONE
        !arguments
        INTEGER, INTENT(IN) :: n,m,o,p
        REAL(8), DIMENSION(n,m,o,p), INTENT(IN) :: matrix
        CHARACTER(*), INTENT(IN), OPTIONAL :: name, format_type
        !local variables
        CHARACTER(20) :: FORMAT
        CHARACTER(100) :: matname
        CHARACTER(10) :: fmt_type
        INTEGER :: i,j,k,l

        matname = "unnamed matrix"
        IF (PRESENT(name)) matname=name
        fmt_type = "long"
        IF (PRESENT(format_type)) fmt_type=format_type

        CALL format_selector(fmt_type,FORMAT,m)

        WRITE(*,*)      "WRITING ", matname
        DO i=1,n
            DO j=1,m
                WRITE(*,*) i, j
                DO k=1,o
                        WRITE(*,FORMAT) (matrix(i,j,k,l),l=1,p)
                END DO
            END DO
        ENDDO
        WRITE(*,*)

    END SUBROUTINE write_matrix4


    SUBROUTINE format_selector(fmt_type_in, fmt_string, amount)
        IMPLICIT NONE
        !arguments
        CHARACTER(*), INTENT(IN)   :: fmt_type_in
        INTEGER, INTENT(IN)         :: amount !amount of numbers to write
        CHARACTER(20), INTENT(OUT) :: fmt_string
        !local variables
        !-

        SELECT CASE (fmt_type_in)
            CASE ("short")
                WRITE(fmt_string,'("(",I0,"E10.2)")') amount
            CASE ("long")
                WRITE(fmt_string,'("(",I0,"E20.8)")') amount
            CASE ("decimal")
                WRITE(fmt_string,'("(",I0,"F10.5)")') amount
            CASE DEFAULT
                WRITE(*,*)
                WRITE(*,*) "WRONG FORMAT_TYPE, ADVANCING WITH long"
                WRITE(fmt_string,'("(",I0,"E20.8)")') amount
        END SELECT

    END SUBROUTINE format_selector

END MODULE
