module more_shapes
    use shapes
    implicit none
    private

    type, public, extends(shape_base) :: rectangle
        private
        real(8) :: hlength
        real(8) :: vlength

    contains

        ! no puedo reemplazar el 'init' the 'shape_base' porque los argumentos
        ! no son equivalentes (eso es porque en 'shape_base' tengo como
        ! argumento opcional a 'length'
        ! aca tengo que tenerlo tambien, es decir, puedo expandir los
        ! argumentos pero no puedo modificar los que ya estan
        procedure :: initr => init_rect
        procedure :: describe => print_rect ! reemplazo el metodo de 'shape_base'

    end type rectangle

contains

    subroutine init_rect(this, name, color, hlength, vlength)
        implicit none
        class(rectangle), intent(out) :: this
        character(len=20), intent(in) :: name, color
        real(8), intent(in) :: hlength, vlength

        call this%shape_base%init(name, color)
        this%hlength = hlength
        this%vlength = vlength

    end subroutine

    subroutine print_rect(this)
        implicit none
        class(rectangle), intent(in) :: this

        !call this%shape_base%describe()
        write(6,*) 'Rectangle printing'
        write(6,*) 'Color: ', this%get_color()
        write(6,*) 'H Length: ', this%hlength
        write(6,*) 'V Length: ', this%vlength

    end subroutine

end module more_shapes
