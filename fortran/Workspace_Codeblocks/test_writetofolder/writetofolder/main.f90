program hello
    implicit none
    integer :: fid

    fid = 10
    open(unit=fid, file='output/out.txt', status='replace')

    write(fid, *) 'Hello World!'

    close(unit=fid)


end program

