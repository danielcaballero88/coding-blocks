program hello
    implicit none
    integer :: i_numvarpar
    integer, allocatable :: iv_n(:), iv_comb(:), iv_comb_old(:)
    integer :: i_ncomb
    integer :: j, k
    integer :: i_old, i_new, i_max, i_pre_old, i_pre
    integer :: i_verbose=2

    i_numvarpar = 4
    allocate( iv_n(i_numvarpar) )
    allocate( iv_comb(i_numvarpar) )
    allocate( iv_comb_old(i_numvarpar) )
    iv_n = [2, 3, 1, 2]
    if (i_verbose>0) print*, "iv_n: ", iv_n

    i_ncomb = product(iv_n)
    if (i_verbose>0) print*, "i_ncomb: ", i_ncomb

    DO j=1,i_ncomb
        IF (j==1) THEN
            iv_comb = 1
            if (i_verbose>0) print*, "iv_comb: ", iv_comb
        ELSE
            CALL contador(i_numvarpar, iv_n, iv_comb)
            if (i_verbose>1) print*, "iv_comb: ", iv_comb

!            iv_comb_old = iv_comb
!            ! el primer indice primero
!            i_old = iv_comb(1)
!            i_new = i_old+1 ! candidato
!            i_max = iv_n(1)
!            ! calculo el nuevo valor
!            IF (i_new > i_max)  THEN
!                ! si supere el maximo debo resetear el indice
!                i_new = 1
!            END IF
!            iv_comb(1) = i_new
!            ! ahora calculo los siguientes indices
!            DO k=2,i_numvarpar
!                ! guardo valores auxiliares utiles
!                i_old = iv_comb(k)
!                i_new = i_old + 1 ! candidato
!                i_max = iv_n(k)
!                i_pre_old = iv_comb_old(k-1)
!                i_pre = iv_comb(k-1)
!                ! chequeo si el indice previo (contiguo a la izquierda) se ha reseteado
!                IF ( i_pre<i_pre_old) THEN
!                    ! si el indice previo ha reseteado debo cambiar
!                    IF (i_new > i_max)  THEN
!                        ! si supere el maximo debo resetear
!                        i_new = 1
!                    END IF
!                ELSE
!                    ! si el indice previo no ha reseteado debo seguir igual
!                    i_new = i_old
!                END IF
!                iv_comb(k) = i_new
!            END DO
!            if (i_verbose>1) print*, "iv_comb: ", iv_comb
        END IF

    END DO

    if (i_verbose>0) print*, iv_comb

end program

SUBROUTINE contador(i_numind, i_v_maxind, i_v_comb)
    ! subrutina para desdoblar bucles anidados en uno solo
    ! es util cuando no se sabe la cantidad de bucles a anidar de antemano
    ! como en el caso que hay que recorrer los elementos de un array
    ! multidimensional allocatable
    ! es util imaginarse un odometro
    IMPLICIT NONE
    !
    INTEGER, INTENT(IN) :: i_numind ! numero de indices a simular
    INTEGER, INTENT(IN) :: i_v_maxind(i_numind) ! numero maximo de valor por indice
    INTEGER, INTENT(INOUT) :: i_v_comb(i_numind)
    !
    LOGICAL :: b_v_reset(i_numind)
    INTEGER :: i_old, i_new, i_max, i_pre
    INTEGER :: k
    !

    ! inicializo
    b_v_reset = .FALSE.

    ! chequeo el cambio del primer indice primero
    i_old = i_v_comb(1)
    i_new = i_old+1 ! candidato
    i_max = i_v_maxind(1)
    ! calculo el nuevo valor segun las opciones
    IF (i_new > i_max)  THEN
        ! si supere el maximo debo resetear el indice
        i_new = 1
        b_v_reset(1) = .TRUE.
    END IF
    i_v_comb(1) = i_new
    ! ahora calculo los siguientes indices
    DO k=2,i_numind
        ! guardo valores auxiliares utiles
        i_old = i_v_comb(k)
        i_new = i_old + 1 ! candidato
        i_max = i_v_maxind(k)
        i_pre = i_v_comb(k-1)
        ! chequeo si el indice previo (contiguo a la izquierda) se ha reseteado
        IF ( b_v_reset(k-1) ) THEN
            ! si el indice previo ha reseteado debo cambiar
            IF (i_new > i_max)  THEN
                ! si supere el maximo debo resetear
                i_new = 1
                b_v_reset(k) = .TRUE.
            END IF
        ELSE
            ! si el indice previo no ha reseteado debo seguir igual
            i_new = i_old
        END IF
        i_v_comb(k) = i_new
    END DO

END SUBROUTINE contador
