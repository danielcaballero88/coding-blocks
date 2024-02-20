program hello
    use shapes
    use more_shapes
    implicit none
    type(square) :: s
    type(rectangle) :: r

    call s%describe()
    call s%init('mi cuadrado;', 'rojo;', 2.5d0)
    call s%describe()
    call s%expand(4.d0)
    call s%describe()

    call r%initr('mi rectangulo;', 'verde;', 1.5d0, 4.d0)
    call r%describe()

end program
