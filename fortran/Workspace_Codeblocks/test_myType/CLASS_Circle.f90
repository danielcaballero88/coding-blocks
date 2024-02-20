MODULE CLASS_Circle

    IMPLICIT NONE
    PRIVATE

    REAL(8), PARAMETER :: PI = 3.1415926535897931d0

    PUBLIC :: Circle

    TYPE Circle
        REAL(8) :: Radius = 1.d0
        !
    CONTAINS
        !
        PROCEDURE :: area  => circle_area
        PROCEDURE :: print => circle_print
    END TYPE Circle

    ! NOTE_avoid_public_variables_if_possible

CONTAINS


    FUNCTION circle_area(self) RESULT(Area)

        CLASS(Circle), INTENT(IN) :: self
        REAL(8) :: Area

        Area = PI*self%Radius**2

    END FUNCTION circle_area

    SUBROUTINE circle_print(self)
        CLASS(Circle), INTENT(IN) :: self

        PRINT *, 'Circle: r = ', self%Radius, ' area = ', self%area()

    END SUBROUTINE

END MODULE CLASS_Circle
