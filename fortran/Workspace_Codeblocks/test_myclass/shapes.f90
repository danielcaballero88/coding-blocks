! ==========
module shapes
! ==========
    ! ==========
    implicit none
    ! ==========
    private
    ! ==========

    type, public :: shape_base
        private
        logical :: inited
        character(len=20) :: name
        character(len=20) :: color

    contains

        procedure :: init => init
        procedure :: describe => print_shape_info
        procedure :: get_color => get_color

    end type

    type, public, extends(shape_base) :: square
        private
        real(8) :: length = 0.d0
        real(8) :: area

    contains

        procedure :: expand => expand_length

    end type

! ==========
contains
! ==========

    ! ----------
    subroutine init(this, name, color, length)
        ! ----------
        implicit none
        ! ----------
        class(shape_base), intent(out) :: this
        character(len=20), intent(in) :: name
        character(len=20), intent(in) :: color
        real(8), optional, intent(in) :: length
        ! ----------

        this%name = cutstr(name)
        this%color = cutstr(color)

        select type (this)
            class is (square)
                this%length = length
                this%area = length*length
        end select

        ! ----------
    end subroutine
    ! ----------

    ! ----------
    function get_color(this) result(color)
        implicit none
        class(shape_base), intent(in) :: this
        character(len=20) :: color

        color = this%color

    end function
    ! ----------

    ! ----------
    subroutine print_shape_info(this)
        ! ----------
        implicit none
        ! ----------
        class(shape_base), intent(in) :: this
        ! ----------

        ! ---
        write(6,*) 'Shape: ', this%name
        write(6,*) 'Color: ', this%color
        ! ---
        select type (this)
        class is (square)
            write(6,*) 'Is a: square'
            write(6,*) 'Length: ', this%length
            write(6,*) 'Area: ', this%area
        class default
        end select
        ! ---

        ! ----------
    end subroutine
    ! ----------

    ! ----------
    subroutine expand_length(this, factor)
        implicit none
        class(square), intent(inout) :: this
        real(8) :: factor

        this%length = this%length*factor
        this%area = this%area*factor*factor

    end subroutine
    ! ----------

    ! ----------
    function cutstr(s_in) result(s_out)
        implicit none
        character(len=20), intent(in) :: s_in
        character(len=20) :: s_out
        integer :: i

        i = index(s_in, ';')
        if (i==0) then
            s_out = s_in
        else
            s_out = s_in(1:i-1) // repeat(' ',20-(i-1))
        end if

    end function
    ! ----------

! ==========
end module
! ==========
