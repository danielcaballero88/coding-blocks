! ==========
module tv
! ==========
    ! ==========
    implicit none
    ! ==========

    ! ----------
    type, public :: circuit_type ! base class
        logical :: is_on = .false.
    end type circuit_type
    ! ----------

    ! ----------
    type, public, extends(circuit_type) :: tv_type ! class derived from circuit
        private
        integer :: channel = 0
        integer :: volume = 0
        integer :: tvkind = 0 ! 0 = led, 2 = oled, 3 = plasma

        contains

        procedure :: power_button => Turn_on_off
        procedure :: print_status => describe

    end type tv_type
    ! ----------

    ! ----------
    type, extends(tv_type) :: led_tv_type ! derived from tv_type
        ! nada
    end type

    ! ----------
    type, extends(tv_type) :: oled_tv_type
        ! nada
    end type
    ! ----------

    ! ----------
    type, extends(tv_type) :: plasma_tv_type
        ! nada
    end type
    ! ----------

    ! ==========
    contains
    ! ==========

    subroutine describe(tv)
        class(tv_type) :: tv
        write(*,*) "Tv is: ", tv%is_on
        write(*,*) "Channel: ", tv%channel
        write(*,*) "Volume: ", tv%volume
    end subroutine

    ! ----------
    subroutine set_TVKind(TV)
        ! se usa "class" aqui debajo en vez de "type" debido al polimorfismo
        ! (TV puede ser de diferentes tipos que heredan a tv_type)
        class(tv_type) :: TV
        TV%tvkind = 0

        select type (TV)
            class is (led_tv_type)
                TV%tvkind = 0
            class is (oled_tv_type)
                TV%tvkind = 1
            class is (plasma_tv_type)
                TV%tvkind = 2
            class default
                stop "Unkown type in set_TVKind"
        end select

    end subroutine set_TVKind
    ! ----------

    ! ----------
    subroutine Init(TV)
        class(tv_type) :: TV
        TV%channel = 0
        TV%volume = 0
        TV%circuit_type%is_on = .false.
    end subroutine Init
    ! ----------

    ! ----------
    subroutine Turn_on_off(TV, L)
        class(tv_type) :: TV
        logical :: L
        TV%circuit_type%is_on = L
    end subroutine
    ! ----------

    ! ----------
    subroutine set_volume(TV, V)
        class(tv_type) :: TV
        integer :: V
        TV%volume = V
    end subroutine
    ! ----------

    ! ----------
    function get_volume(TV) result(vol)
        class(tv_type) :: TV
        integer :: vol
        vol = TV%volume
    end function
    ! ----------

! ==========
end module tv
! ==========
