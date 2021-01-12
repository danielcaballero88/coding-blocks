program testarray
    implicit none
    integer, parameter :: asize=5000
    real, dimension(asize,asize) :: array

    integer :: i, j
    integer :: time, u

    forall (i=1:asize, j=1:asize) array(i,j)=i*asize+j

    call tick(time)
    open(newunit=u,file='test.txt')
    do i=1,asize
        write(u,*) (array(i,j), j=1,asize)
    enddo
    close(u)
    print *, 'ASCII: time = ', tock(time)

    call tick(time)
    open(newunit=u,file='test.dat',form='unformatted')
    write(u) array
    close(u)
    print *, 'Binary: time = ', tock(time)

    call tick(time)
    call writenetcdffile(array)
    print *, 'NetCDF: time = ', tock(time)


contains
    subroutine tick(t)
        integer, intent(OUT) :: t
        call system_clock(t)
    end subroutine tick

    ! returns time in seconds from now to time described by t
    real function tock(t)
        integer, intent(in) :: t
        integer :: now, clock_rate
        call system_clock(now,clock_rate)
        tock = real(now - t)/real(clock_rate)
    end function tock

    subroutine writenetcdffile(array)
        use netcdf
        implicit none
        real, intent(IN), dimension(:,:) :: array

        integer :: file_id, xdim_id, ydim_id
        integer :: array_id
        integer, dimension(2) :: arrdims
        character(len=*), parameter :: arrunit = 'ergs'

        integer :: i, j
        integer :: ierr

        i = size(array,1)
        j = size(array,2)

        ! create the file
        ierr = nf90_create(path='test.nc', cmode=NF90_CLOBBER, ncid=file_id)

        ! define the dimensions
        ierr = nf90_def_dim(file_id, 'X', i, xdim_id)
        ierr = nf90_def_dim(file_id, 'Y', j, ydim_id)

        ! now that the dimensions are defined, we can define variables on them,...
        arrdims = (/ xdim_id, ydim_id /)
        ierr = nf90_def_var(file_id, 'Array',  NF90_REAL, arrdims, array_id)

        ! ...and assign units to them as an attribute
        ierr = nf90_put_att(file_id, array_id, "units", arrunit)

        ! done defining
        ierr = nf90_enddef(file_id)

        ! Write out the values
        ierr = nf90_put_var(file_id, array_id, array)

        ! close; done
        ierr = nf90_close(file_id)
    return
    end subroutine writenetcdffile
end program testarray
