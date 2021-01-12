
PROGRAM hello
    USE CLASS_Circle
    IMPLICIT NONE

    TYPE(Circle) :: c
    REAL(8) :: a

    PRINT *, "Hello World!"

    ! c = Circle()
    call c%print()
    c%Radius = 10.d0
    call c%print()

    a = c%area()
    print *, a

END PROGRAM

