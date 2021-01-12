! fortran library test
! compile with the following flags
! gfortran -Wall -shared -fPIC -o libtest.so libtest.f90
!

subroutine test1 (a) BIND(C, name='test1')
      use iso_c_binding
      implicit none

      integer(4), intent(in) :: a

      write(*,*) 'hello from fortran subroutine test1'
      write(*,*) 'a = ', a

end subroutine


subroutine test2 (a) BIND(C, name='test2')
      use iso_c_binding
      implicit none

      integer(4), intent(in) :: a

      write(*,*) 'hello from fortran subroutine: test2'
      write(*,*) 'a = ', a

end subroutine 
