program main
    USE mod_algebra
	implicit none
	INTEGER, PARAMETER :: i_np = 11
	REAL(8) :: rv_x(3), rv_y(i_np)
	REAL(8) :: r_x, r_y
	INTEGER :: j

	rv_x = [0.5, 1.5, 0.1]
	DO j=1,i_np
		r_x = rv_x(1) + DFLOAT(j-1)*rv_x(3)
		rv_y(j) = r_x**2
	END DO

	r_x = 1.0
	r_y = ComputeFromCurve(r_x, i_np, rv_x, rv_y, .TRUE.)

    print *, r_y

end program
