
program test_pointer
    implicit none
    INTEGER, PARAMETER  :: l_v=10
    INTEGER, POINTER    :: v_ptr(:)
    INTEGER, TARGET     :: v(l_v)
    INTEGER, TARGET     :: v2(2*l_v)
    INTEGER             :: i

    print *, "PROGRAM TO TEST POINTERS AS ALLOCATABLES IN FORTRAN"

    DO i=1,l_v
        v(i) = i
    END DO

    write(6,*) "v= ",v

    v_ptr => v

    write(6,*) "v_ptr= ",v_ptr

    DO i=1,2*l_v
        v2(i) = i
    END DO

    v_ptr => v2

    write(6,*) "v_ptr= ",v_ptr

    ALLOCATE( v_ptr(5) )

    DO i=1,5
        v_ptr(i) = i
    END DO

    write(6,*) "v_ptr= ",v_ptr

    ALLOCATE( v_ptr(9) )

    DO i=1,9
        v_ptr(i) = i
    END DO

    write(6,*) "v_ptr= ",v_ptr

    print *, "CONCLUSION: POINTERS WORK AS ALLOCATABLES BUT WITH MORE FLEXIBILITY"

end program

