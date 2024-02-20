module LIB_WriteToVTK

!----------------------------------------------------------------------
implicit none
private
! functions for VTK LEGACY
public:: VTK_INI
public:: VTK_GEO_UNST_R8
public:: VTK_CON
public:: VTK_DAT
public:: VTK_VAR_SCAL_R8
public:: VTK_VAR_VECT_R8
public:: VTK_END
public:: GetUnit
public:: Upper_Case
public:: iFindStringInFile

!!Real precision definitions:
!!
integer, parameter:: R8P  = selected_real_kind(15,307)  ! 15  digits, range $[\pm 10^{-307}~~ ,\pm 10^{+307}~~  -1]$
!!Integer precision definitions:
!!
integer, parameter:: I8P  = selected_int_kind(18)       ! range $[-2^{63} ,+2^{63}  -1]$
integer, parameter:: I4P  = selected_int_kind(9)        ! range $[-2^{31} ,+2^{31}  -1]$
!!
!!Besides the kind parameters there are also the format parameters useful for writing in a well-ascii-format numeric variables.
!!Also these parameters are public.
!!
!! Real output formats:
!!
!character(10), parameter:: FR8P  = '(E23.15E3)'         ! R8P   output format
character(10), parameter:: FR8P  = '(E17.8E3)'
!! Integer output formats:
!!
character(5), parameter:: FI4P  = '(I12)'               ! I4P  output format
!!
!!
!!\LIBVTKIO uses a small set of internal variables that are private (not accessible from the outside). The following are
!! private variables:
!!
integer(I4P), parameter:: maxlen       = 500         ! max number of characters os static string
character(1), parameter:: end_rec      = char(10)    ! end-character for binary-record finalize
integer(I4P), parameter:: f_out_ascii  = 0           ! ascii-output-format parameter identifier
integer(I4P), parameter:: f_out_binary = 1           ! binary-output-format parameter identifier
integer(I4P)::            f_out        = f_out_ascii ! current output-format (initialized to ascii format)
character(len=maxlen)::   topology                   ! mesh topology
integer(I4P)::            Unit_VTK                   ! internal logical unit
!----------------------------------------------------------------------------------------------------------------------------------
contains
!!\LIBVTKIO uses two auxiliary functions that are not connected with the VTK standard. These functions are private and so they
!!cannot be called outside the library.

function GetUnit() result(Free_Unit)
!--------------------------------------------------------------------------------------------------------------------------------
!!The GetUnit function is used for getting a free logic unit. The users of \LIBVTKIO does not know which is
!!the logical unit: \LIBVTKIO handels this information without boring the users. The logical unit used is safe-free: if the
!!program calling \LIBVTKIO has others logical units used \LIBVTKIO will never use these units, but will choice one that is free.
!--------------------------------------------------------------------------------------------------------------------------------

implicit none

!--------------------------------------------------------------------------------------------------------------------------------
integer(I4P):: Free_Unit ! free logic unit
integer(I4P):: n1        ! counter
integer(I4P):: ios       ! inquiring flag
logical(4)::   lopen     ! inquiring flag
!--------------------------------------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------------------------------------
!!The following is the code snippet of GetUnit function: the units 0, 5, 6, 9 and all non-free units are discarded.
!!
!(\doc)codesnippet
Free_Unit = -1_I4P                                      ! initializing free logic unit
n1=1_I4P                                                ! initializing counter
do
  if ((n1/=5_I4P).AND.(n1/=6_I4P).AND.(n1/=9_I4P)) then
    inquire (unit=n1,opened=lopen,iostat=ios)           ! verify logic units
    if (ios==0_I4P) then
      if (.NOT.lopen) then
        Free_Unit = n1                                  ! assignment of free logic
        return
      endif
    endif
  endif
  n1=n1+1_I4P                                           ! updating counter
enddo
return
!!GetUnit function is private and cannot be called outside \LIBVTKIO. If you are interested to use it change its scope to public.
!--------------------------------------------------------------------------------------------------------------------------------
end function GetUnit

function Upper_Case(string)

!--------------------------------------------------------------------------------------------------------------------------------
!!The Upper_Case function converts the lower case characters of a string to upper case one. \LIBVTKIO uses this function in
!!order to achieve case-insensitive: all character variables used within \LIBVTKIO functions are pre-processed by
!!Uppper_Case function before these variables are used. So the users can call \LIBVTKIO functions whitout pay attention of the
!!case of the kwywords passed to the functions: calling the function VTK_INI with the string \code{E_IO = VTK_INI('Ascii',...)}
!!or with the string  \code{E_IO = VTK_INI('AscII',...)} is equivalent.
!--------------------------------------------------------------------------------------------------------------------------------

implicit none

!--------------------------------------------------------------------------------------------------------------------------------
character(len=*), intent(IN):: string     ! string to be converted
character(len=len(string))::   Upper_Case ! converted string
integer::                      n1         ! characters counter
!--------------------------------------------------------------------------------------------------------------------------------


Upper_Case = string
do n1=1,len(string)
  select case(ichar(string(n1:n1)))
  case(97:122)
    Upper_Case(n1:n1)=char(ichar(string(n1:n1))-32) ! Upper case conversion
  end select
enddo
return


!--------------------------------------------------------------------------------------------------------------------------------
end function Upper_Case

function VTK_INI(output_format,filename,title,mesh_topology) result(E_IO)

implicit none

character(*), intent(IN):: output_format ! output format: ASCII or BINARY
character(*), intent(IN):: filename      ! name of file
character(*), intent(IN):: title         ! title
character(*), intent(IN):: mesh_topology ! mesh topology
integer(I4P)::             E_IO          ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done


!!The following is an example of VTK_INI calling:
!!
!!E_IO = VTK_INI('Binary','example.vtk','VTK legacy file','UNSTRUCTURED_GRID')

!!Note that the suffix '.vtk' extension is necessary in the file name.
!--------------------------------------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------------------------------------
topology = trim(mesh_topology)
Unit_VTK=GetUnit()
select case(trim(Upper_Case(output_format)))
case('ASCII')
  f_out = f_out_ascii
  open(unit     = Unit_VTK,       &
       file     = trim(filename), &
       form     = 'FORMATTED',    &
       access   = 'SEQUENTIAL',   &
       action   = 'WRITE',        &
!       buffered = 'YES',          &
       iostat   = E_IO)
  ! writing header of file
  write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'# vtk DataFile Version 3.0'
  write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)trim(title)
  write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)trim(Upper_Case(output_format))
  write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'DATASET '//trim(topology)
!case('BINARY')
!  f_out = f_out_binary
!  open(unit       = Unit_VTK,       &
!       file       = trim(filename), &
!       form       = 'UNFORMATTED',  &
!       access     = 'SEQUENTIAL',   &
!       action     = 'WRITE',        &
!       convert    = 'BIG_ENDIAN',   &
!       recordtype = 'STREAM',       &
!       buffered   = 'YES',          &
!       iostat     = E_IO)
!  ! writing header of file
!  write(unit=Unit_VTK,iostat=E_IO)'# vtk DataFile Version 3.0'//end_rec
!  write(unit=Unit_VTK,iostat=E_IO)trim(title)//end_rec
!  write(unit=Unit_VTK,iostat=E_IO)trim(Upper_Case(output_format))//end_rec
!  write(unit=Unit_VTK,iostat=E_IO)'DATASET '//trim(topology)//end_rec
end select
return
!--------------------------------------------------------------------------------------------------------------------------------



end function VTK_INI

function VTK_GEO_UNST_R8(NN,X,Y,Z) result(E_IO)
!--------------------------------------------------------------------------------------------------------------------------------
!! Function for saving mesh; topology = UNSTRUCTURED_GRID (R8P).
!--------------------------------------------------------------------------------------------------------------------------------

implicit none

!--------------------------------------------------------------------------------------------------------------------------------
integer(I4P), intent(IN):: NN        ! number of nodes
real(R8P),    intent(IN):: X(1:NN)   ! x coordinates of all nodes
real(R8P),    intent(IN):: Y(1:NN)   ! y coordinates of all nodes
real(R8P),    intent(IN):: Z(1:NN)   ! z coordinates of all nodes
integer(I4P)::             E_IO      ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
!character(len=maxlen)::    s_buffer  ! buffer string
integer(I4P)::             n1        ! counter
!--------------------------------------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------------------------------------

write(unit=Unit_VTK,fmt='(A,'//FI4P//',A)',iostat=E_IO)'POINTS ',NN,' double'
write(unit=Unit_VTK,fmt='(3'//FR8P//')',   iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)

!select case(f_out)
!case(f_out_ascii)
!  write(unit=Unit_VTK,fmt='(A,'//FI4P//',A)',iostat=E_IO)'POINTS ',NN,' double'
!  write(unit=Unit_VTK,fmt='(3'//FR8P//')',   iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
!case(f_out_binary)
!  write(s_buffer,     fmt='(A,'//FI4P//',A)',iostat=E_IO)'POINTS ',NN,' double'
!  write(unit=Unit_VTK,                       iostat=E_IO)trim(s_buffer)//end_rec
!  write(unit=Unit_VTK,                       iostat=E_IO)(X(n1),Y(n1),Z(n1),n1=1,NN)
!  write(unit=Unit_VTK,                       iostat=E_IO)end_rec
!end select
return
!--------------------------------------------------------------------------------------------------------------------------------
end function VTK_GEO_UNST_R8

function VTK_CON(NC,connect,cell_type) result(E_IO)
!--------------------------------------------------------------------------------------------------------------------------------
!!This function must be used when unstructured grid is used. It saves the connectivity of the unstructured
!!mesh.
!--------------------------------------------------------------------------------------------------------------------------------

implicit none

!--------------------------------------------------------------------------------------------------------------------------------
integer(I4P), intent(IN):: NC              ! number of cells
integer(I4P), intent(IN):: connect(:)      ! mesh connectivity
integer(I4P), intent(IN):: cell_type(1:NC) ! VTK cell type
integer(I4P)::             E_IO            ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
!character(len=maxlen)::    s_buffer        ! buffer string
integer(I4P)::             ncon            ! dimension of connectivity vector 
!!The VTK\_CON variables have the following meaning:
!!

!!\noindent where $dc$ is connectivity vector dimension and $nvertex_i$ is the number of vertices of $i^{th}$ cell. The VTK
!!legacy standard for the mesh connectivity is quite obscure at least at first sight. It is more simple analizing an example.
!!Suppose we have a mesh composed by 2 cells, one hexahedron (8 vertices) and one pyramid with square basis (5 vertices); suppose
!!that the basis of pyramid is constitute by a face of the hexahedron and so the two cells share 4 vertices. The equation
!!\ref{eq:connectivity dimensions} gives $dc=2+8+5=15$; the connectivity vector for this mesh can be:
!!
!!Connectivity vector example for VTK legacy standard
!!
!!! first cell
!!connect(1)  = 8  => number of vertices of 1° cell
!!connect(2)  = 0  => identification flag of 1° vertex of 1° cell
!!connect(3)  = 1  => identification flag of 2° vertex of 1° cell
!!connect(4)  = 2  => identification flag of 3° vertex of 1° cell
!!connect(5)  = 3  => identification flag of 4° vertex of 1° cell
!!connect(6)  = 4  => identification flag of 5° vertex of 1° cell
!!connect(7)  = 5  => identification flag of 6° vertex of 1° cell
!!connect(8)  = 6  => identification flag of 7° vertex of 1° cell
!!connect(9)  = 7  => identification flag of 8° vertex of 1° cell
!!! second cell
!!connect(10) = 5  => number of vertices of 2° cell
!!connect(11) = 0  => identification flag of 1° vertex of 2° cell
!!connect(12) = 1  => identification flag of 2° vertex of 2° cell
!!connect(13) = 2  => identification flag of 3° vertex of 2° cell
!!connect(14) = 3  => identification flag of 4° vertex of 2° cell
!!connect(15) = 8  => identification flag of 5° vertex of 2° cell
!!
!!Note that the first 4 identification flags of pyramid vertices as the same of the first 4 identification flags of
!!the hexahedron because the two cells share this face. It is also important to note that the identification flags start
!!form $0$ value: this is impose to the VTK standard. The function VTK_CON does not calculate the connectivity vector: it
!!writes the connectivity vector conforming the VTK standard, but does not calculate it. In the future release of \LIBVTKIO will
!!be included a function to calculate the connectivity vector.
!!
!!The following is an example of VTK_CON calling:
!!
!!integer(4), parameter:: NC=2
!!integer(4), parameter:: Nvertex1=8
!!integer(4), parameter:: Nvertex2=5
!!integer(4), parameter:: dc=NC+Nvertex1+Nvertex2
!!integer(4)::            connect(1:dc)
!!integer(4)::            cell_type(1:NC)
!!...
!!E_IO = VTK_CON(NC,connect,cell_type)

!--------------------------------------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------------------------------------

ncon = size(connect,1)

write(unit=Unit_VTK,fmt='(A,2'//FI4P//')',iostat=E_IO)'CELLS ',NC,ncon
write(unit=Unit_VTK,fmt=FI4P,             iostat=E_IO)connect
write(unit=Unit_VTK,fmt='(A,'//FI4P//')', iostat=E_IO)'CELL_TYPES ',NC
write(unit=Unit_VTK,fmt=FI4P,             iostat=E_IO)cell_type

!ncon = size(connect,1)
!select case(f_out)
!case(f_out_ascii)
!  write(unit=Unit_VTK,fmt='(A,2'//FI4P//')',iostat=E_IO)'CELLS ',NC,ncon
!  write(unit=Unit_VTK,fmt=FI4P,             iostat=E_IO)connect
!  write(unit=Unit_VTK,fmt='(A,'//FI4P//')', iostat=E_IO)'CELL_TYPES ',NC
!  write(unit=Unit_VTK,fmt=FI4P,             iostat=E_IO)cell_type
!case(f_out_binary)
!  write(s_buffer,     fmt='(A,2'//FI4P//')',iostat=E_IO)'CELLS ',NC,ncon
!  write(unit=Unit_VTK,                      iostat=E_IO)trim(s_buffer)//end_rec
!  write(unit=Unit_VTK,                      iostat=E_IO)connect
!  write(unit=Unit_VTK,                      iostat=E_IO)end_rec
!  write(s_buffer,     fmt='(A,'//FI4P//')', iostat=E_IO)'CELL_TYPES ',NC
!  write(unit=Unit_VTK,                      iostat=E_IO)trim(s_buffer)//end_rec
!  write(unit=Unit_VTK,                      iostat=E_IO)cell_type
!  write(unit=Unit_VTK,                      iostat=E_IO)end_rec
!end select
return
!--------------------------------------------------------------------------------------------------------------------------------

end function VTK_CON

function VTK_DAT(NC_NN,var_location) result(E_IO)
!--------------------------------------------------------------------------------------------------------------------------------
!!This function must be called before saving the data related to geometric mesh. This function initializes the
!!saving of data variables indicating the type of variables that will be saved.
!--------------------------------------------------------------------------------------------------------------------------------

implicit none

!--------------------------------------------------------------------------------------------------------------------------------
integer(I4P), intent(IN):: NC_NN        ! number of cells or nodes of field
character(*), intent(IN):: var_location ! location of saving variables: cell for cell-centered, node for node-centered
integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
!character(len=maxlen)::    s_buffer     ! buffer string
!!The VTK\_DAT variables have the following meaning:
!!
!!\begin{description}
!! NC_NN indicates the number of all cells or all nodes according to the value of tipo.
!! var_location contains the location-type of variables that will be saved after VTK\_DAT. It is a scalar and cab assume the following values:
!!
!! #1  'cell' (it is case insensitive) --> variables will be cell-centered.
!! #2  'node' (it is case insensitive) --> variables will be node-centered.
!!
!! \item[{\color{RoyalBlue}E\_IO}] contains the inquiring integer flag for error handling.
!!\end{description}
!!
!!Of course a single file can contain both cell and node centered variables; in this case the VTK\_DAT function must be called two times, before saving cell-centered variables and before saving node-centered variables.
!!
!!The following is an example of VTK_DAT calling:
!!
!!VTK\_DAT Calling
!!
!!...
!!E_IO = VTK_DAT(50,'node')
!!...
!--------------------------------------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------------------------------------

select case(trim(Upper_Case(var_location)))
case('CELL')
  write(unit=Unit_VTK,fmt='(A,'//FI4P//')',iostat=E_IO)'CELL_DATA ',NC_NN
case('NODE')
  write(unit=Unit_VTK,fmt='(A,'//FI4P//')',iostat=E_IO)'POINT_DATA ',NC_NN
end select

!select case(f_out)
!case(f_out_ascii)
!  select case(trim(Upper_Case(var_location)))
!  case('CELL')
!    write(unit=Unit_VTK,fmt='(A,'//FI4P//')',iostat=E_IO)'CELL_DATA ',NC_NN
!  case('NODE')
!    write(unit=Unit_VTK,fmt='(A,'//FI4P//')',iostat=E_IO)'POINT_DATA ',NC_NN
!  end select
!case(f_out_binary)
!  select case(trim(Upper_Case(var_location)))
!  case('CELL')
!    write(s_buffer,     fmt='(A,'//FI4P//')',iostat=E_IO)'CELL_DATA ',NC_NN
!    write(unit=Unit_VTK,                     iostat=E_IO)trim(s_buffer)//end_rec
!  case('NODE')
!    write(s_buffer,     fmt='(A,'//FI4P//')',iostat=E_IO)'POINT_DATA ',NC_NN
!    write(unit=Unit_VTK,                     iostat=E_IO)trim(s_buffer)//end_rec
!  end select
!end select
return
!--------------------------------------------------------------------------------------------------------------------------------
end function VTK_DAT

function VTK_VAR_SCAL_R8(NC_NN,varname,var) result(E_IO)
!--------------------------------------------------------------------------------------------------------------------------------
!! Function for saving field of scalar variable (R8P).
!--------------------------------------------------------------------------------------------------------------------------------

implicit none

!--------------------------------------------------------------------------------------------------------------------------------
integer(I4P), intent(IN):: NC_NN        ! number of nodes or cells
character(*), intent(IN):: varname      ! variable name
real(R8P),    intent(IN):: var(1:NC_NN) ! variable to be saved
integer(I4P)::             E_IO         ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
!--------------------------------------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------------------------------------

write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'SCALARS '//trim(varname)//' double 1'
write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'LOOKUP_TABLE default'
write(unit=Unit_VTK,fmt=FR8P, iostat=E_IO)var

!select case(f_out)
!case(f_out_ascii)
!  write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'SCALARS '//trim(varname)//' double 1'
!  write(unit=Unit_VTK,fmt='(A)',iostat=E_IO)'LOOKUP_TABLE default'
!  write(unit=Unit_VTK,fmt=FR8P, iostat=E_IO)var
!case(f_out_binary)
!  write(unit=Unit_VTK,iostat=E_IO)'SCALARS '//trim(varname)//' double 1'//end_rec
!  write(unit=Unit_VTK,iostat=E_IO)'LOOKUP_TABLE default'//end_rec
!  write(unit=Unit_VTK,iostat=E_IO)var
!  write(unit=Unit_VTK,iostat=E_IO)end_rec
!end select
return
!--------------------------------------------------------------------------------------------------------------------------------
end function VTK_VAR_SCAL_R8

function VTK_VAR_VECT_R8(vec_type,NC_NN,varname,varX,varY,varZ) result(E_IO)
!--------------------------------------------------------------------------------------------------------------------------------
!! Function for saving field of vectorial variable (R8P).
!--------------------------------------------------------------------------------------------------------------------------------

implicit none

!--------------------------------------------------------------------------------------------------------------------------------
character(*), intent(IN):: vec_type      ! vector type: vect = generic vector , norm = normal vector
integer(I4P), intent(IN):: NC_NN         ! number of nodes or cells
character(*), intent(IN):: varname       ! variable name
real(R8P),    intent(IN):: varX(1:NC_NN) ! x component of vector
real(R8P),    intent(IN):: varY(1:NC_NN) ! y component of vector
real(R8P),    intent(IN):: varZ(1:NC_NN) ! z component of vector
integer(I4P)::             E_IO          ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
integer(I8P)::             n1            ! counter
!--------------------------------------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------------------------------------
select case(Upper_Case(trim(vec_type)))
case('VECT')
  write(unit=Unit_VTK,fmt='(A)',          iostat=E_IO)'VECTORS '//trim(varname)//' double'
case('NORM')
  write(unit=Unit_VTK,fmt='(A)',          iostat=E_IO)'NORMALS '//trim(varname)//' double'
end select
write(unit=Unit_VTK,fmt='(3'//FR8P//')',iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)

!select case(f_out)
!case(f_out_ascii)
!  select case(Upper_Case(trim(vec_type)))
!  case('VECT')
!    write(unit=Unit_VTK,fmt='(A)',          iostat=E_IO)'VECTORS '//trim(varname)//' double'
!  case('NORM')
!    write(unit=Unit_VTK,fmt='(A)',          iostat=E_IO)'NORMALS '//trim(varname)//' double'
!  end select
!  write(unit=Unit_VTK,fmt='(3'//FR8P//')',iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
!case(f_out_binary)
!  select case(Upper_Case(trim(vec_type)))
!  case('VECT')
!    write(unit=Unit_VTK,iostat=E_IO)'VECTORS '//trim(varname)//' double'//end_rec
!  case('NORM')
!    write(unit=Unit_VTK,iostat=E_IO)'NORMALS '//trim(varname)//' double'//end_rec
!  end select
!  write(unit=Unit_VTK,iostat=E_IO)(varX(n1),varY(n1),varZ(n1),n1=1,NC_NN)
!  write(unit=Unit_VTK,iostat=E_IO)end_rec
!end select
return
!--------------------------------------------------------------------------------------------------------------------------------
end function VTK_VAR_VECT_R8

function VTK_END() result(E_IO)
!--------------------------------------------------------------------------------------------------------------------------------
!!This function is used to finalize the file opened and it has not inputs. The \LIBVTKIO manages the file unit without the
!!user's action.
!--------------------------------------------------------------------------------------------------------------------------------

implicit none

!--------------------------------------------------------------------------------------------------------------------------------
integer(I4P):: E_IO ! Input/Output inquiring flag: $0$ if IO is done, $> 0$ if IO is not done
!!The VTK\_END variables have the following meaning:
!!
!!\begin{description}
!! \item[{\color{RoyalBlue}E\_IO}] contains the inquiring integer flag for error handling.
!!\end{description}
!!
!!The following is an example of VTK\_END calling:
!!
!!VTK_END Calling
!!...
!!E_IO = VTK_END()
!!
!--------------------------------------------------------------------------------------------------------------------------------

!--------------------------------------------------------------------------------------------------------------------------------
close(unit=Unit_VTK,iostat=E_IO)
return
!--------------------------------------------------------------------------------------------------------------------------------
end function VTK_END

Function iFindStringInFile(Str,ioUnit)

!--------------------------------------------------------------------------------------------------------------------------------
!! Busca un String en un archivo (STR), sino lo encuentra rebovina el
!! archivo y pone iError < 0 como indicador de no hallazgo
!!
!! Str: String to find, ioUnit: Unit assigned to Input File;
!!                      iError: Error Status variable
!!
!! Author: Santiago Urquiza?
!--------------------------------------------------------------------------------------------------------------------------------

implicit none

!--------------------------------------------------------------------------------------------------------------------------------
Logical,Parameter  :: Segui=.True.
Character(*) Str,DummyString*120
integer :: ioUnit, iFindStringInFile, iError, Leng
!--------------------------------------------------------------------------------------------------------------------------------


iError=0
Leng = Len_Trim(Str)

100 Format(1A120)
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
Search_Loop: do while (segui)
    read(ioUnit,100,iostat=iError)DummyString
!   if (iError==59) backspace(ioUnit)
    if (iError.lt.0) then
        rewind (ioUnit)
        exit Search_Loop
    endif
    if ( DummyString(1:1)    /=    '*'      ) Cycle Search_Loop
    if ( DummyString(1:Leng) == Str(1:Leng) ) Exit Search_Loop
end do Search_Loop
!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

iFindStringInFile = iError

return
end function


end module LIB_WriteToVTK
