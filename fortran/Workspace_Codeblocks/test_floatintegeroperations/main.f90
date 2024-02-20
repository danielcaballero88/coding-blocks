
program hello
    implicit none
    REAL(8) :: r, s
    INTEGER :: i, j

    r = 5.0d0
    i = 2

    s = i/r
    j = i/r

    WRITE(*,*) s, j

end program

