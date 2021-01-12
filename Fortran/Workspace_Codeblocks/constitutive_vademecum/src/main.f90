program hello
    USE class_parametro
    USE mod_io
    USE mod_vademecum
    USE mod_bundle
    USE netCDF
    USE mod_algebra
    IMPLICIT NONE
    ! Parameters
    INTEGER :: i_npLam
    REAL(8) :: rv_Lam(3)
    INTEGER :: i_nPar
    TYPE(Parametro), ALLOCATABLE :: tv_Par(:)

    ! ---
    INTEGER :: i_error, id_vademecum
    CHARACTER(LEN=120) :: s_configFile, s_vademecum
    INTEGER :: var_tensiones
    REAL(8), ALLOCATABLE :: rv_param(:)
    INTEGER, ALLOCATABLE :: iv_param(:)
    INTEGER :: i_nps
    REAL(8), ALLOCATABLE :: rv_tensiones(:)
    INTEGER :: j_param
    REAL(8) :: r_min, r_max, r_del, r_parval
    ! ---



    ! ==========
    ! Leer parametros del archivo de configuracion "vademecum.config"
    s_configFile = "vademecum.config"
    CALL Leer_Configuracion(s_configFile, &
                            i_npLam, rv_Lam, &
                            i_nPar, tv_Par, &
                            s_vademecum)


    ALLOCATE( rv_tensiones(i_npLam) )
    CALL Get_CurvaConstitutiva_Haz(i_npLam, rv_Lam, i_nPar, tv_Par%vals(1), rv_tensiones)
    CALL Escribir_Curva("curva.txt", i_npLam, rv_Lam, rv_tensiones)


!
!    CALL Write_Vademecum(TRIM(s_vademecum), &
!                         i_npLam, rv_Lam, &
!                         i_nPar, tv_Par, &
!                         Get_CurvaConstitutiva_Haz)
!
!
!    ! Ahora voy a leer una curva constitutiva para un set de parametros dados
!    ! Entiendo que ya tengo como conocidos los valores leidos en "vademecum.config"
!    ! ya que ese archivo deberia siempre acompaniar al dataset "vademecum.nc"
!
!    ALLOCATE( rv_tensiones(i_npLam) )
!    ALLOCATE( rv_param(i_nPar) )
!    ALLOCATE( iv_param(i_nPar) )
!
!    ! doy hardcoded los parametros que quiero para la curva constitutiva
!    rv_param = [0.29d10, 0.1d6, 1.105d0, 0.0286d0, 1.0d0, 1.4d0, 10001.0d0]
!    ! me fijo que esos parametros esten dentro de los limites que tengo en el vademecum
!    DO j_param=1,i_nPar
!        r_parval = rv_param(j_param)
!        r_min = tv_Par(j_param)%vals(1)
!        r_max = tv_Par(j_param)%vals(2)
!        r_del = tv_Par(j_param)%vals(3)
!        i_nps = tv_Par(j_param)%np
!        ! si tengo un solo valor de parametro necesito que sea un valor suficientemente cercano
!        IF ( i_nps == 1) THEN
!            IF ( DiferenciaRelativa(r_parval,r_min) < 0.01d0*r_min ) THEN
!                iv_param(j_param) = 1
!            ELSE
!                WRITE(*,*) "Error, valor de parametro muy alejado para unico valor en vademecum"
!                WRITE(*,*) "Parametro: ", j_param
!            END IF
!        ! si tengo varios valores me fijo que caiga dentro de los valores maximo y minimo
!        ELSE
!            IF ( (r_min-r_del*0.5d0<=r_parval) .AND. (r_parval<=r_max+r_del*0.5d0) ) THEN
!                ! tomo el valor mas cercano
!                iv_param(j_param) = CEILING( (r_parval-r_min+0.5d0*r_del)/ r_del )
!            ELSE
!                WRITE(*,*) "Error, valor de parametro por fuera de los limites del vademecum"
!                WRITE(*,*) "Parametro: ", j_param
!            END IF
!        END IF
!    END DO
!
!
!    i_error = nf90_open(TRIM(s_vademecum), nf90_nowrite, id_vademecum)
!    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
!
!    i_error = nf90_inq_varid(id_vademecum, "var_tensiones", var_tensiones)
!    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
!
!    ! tomo los dos valores de curva para los parametros por derecha y por izquierda de los que tengo
!    iv_param = [1,1,1,1,1,1,1]
!    i_error = nf90_get_var(id_vademecum, var_tensiones, rv_tensiones, start=[1,iv_param], stride=[1,1])
!    if(i_error /= nf90_NoErr) print*, NF90_STRERROR(i_error)
!
!    ! la escribo en un archivo
!    CALL Escribir_Curva("curva.txt", i_npLam, rv_Lam, rv_tensiones)


end program






subroutine tick(t)
    real(8), intent(OUT) :: t
    integer :: it
    call system_clock(it)
    t = dfloat(it)
end subroutine tick


! returns time in seconds from now to time described by t
real(8) function tock(t)
    real(8), intent(in) :: t
    real(8) :: now, clock_rate
    integer :: inow, iclock_rate
    call system_clock(inow,iclock_rate)
    now = dfloat(inow)
    clock_rate = dfloat(iclock_rate)
    tock = (now - t)/(clock_rate)
end function tock
