
program hello
    implicit none
    INTEGER :: fid

    print *, "Hello World!"

    fid = 12
    OPEN(UNIT=fid, FILE='csv.csv', STATUS='replace')

end program

