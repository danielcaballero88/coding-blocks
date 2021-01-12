
program hello
    implicit none
    REAL(8) :: r

    print *, "Hello World!"

    CALL mysub1(2.d0,5.d0,r)

    print *, r

    CALL mysub2(2.0d0, mysub1, r)

    PRINT *, r

end program

SUBROUTINE mysub1(a,b,c)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: a,b
    REAL(8), INTENT(OUT) :: c

    c = a*b

END SUBROUTINE

SUBROUTINE mysub2(a,b,c)
    IMPLICIT NONE
    REAL(8), INTENT(IN) :: a
    EXTERNAL :: b
    INTERFACE
        subroutine b(x,y,z)
            REAL(8), INTENT(IN) :: x,y
            REAL(8), INTENT(OUT) :: z
        end subroutine
    END INTERFACE
    REAL(8), INTENT(OUT) :: c

    CALL b(a,a,c)

END SUBROUTINE

