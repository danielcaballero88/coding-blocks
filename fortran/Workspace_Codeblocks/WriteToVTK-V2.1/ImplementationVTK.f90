program ImplementationVTK

    use LIB_WriteToVTK
    implicit none

    !--------------------------------------------------------------------------------------------------------------------------------
    !integer(4), parameter::       Nn   = 8_I4P
    !integer(4), parameter::       Ne   = 1_I4P
    !real(8),    allocatable :: X(:)
    real(8),    allocatable :: x_uns(:)            !dimension Nn
    real(8),    allocatable :: y_uns(:)            !dimension Nn
    real(8),    allocatable :: z_uns(:)            !dimension Nn
    !integer(4), allocatable :: tipo(:)             !dimension Ne
    integer(4), allocatable :: connect(:)          !dimension [ sum(IE) + size(IE) ]  or [ size(IE) + size(JE) ]
    !real(8),    allocatable :: var_uns_grid !dimension Nn
    !real(8),    allocatable :: var_uns_grid_X(:)
    !real(8),    allocatable :: var_uns_grid_Y(:)   ! Nn
    !real(8),    allocatable :: var_uns_grid_Z(:)

    integer(4) ::  E_IO

    integer(4), allocatable :: IDGroup(:), NelTG(:)
    character(30), allocatable :: NelName(:), NodElG(:), DOFNames(:)
    integer(4) Unit_MESH, Unit_DataOUT, Unit_BASPARAM
    integer(4) iError, iDofT, Ndim, NodT, NGROUPS, n1, n2, i, j, k
    integer(4), allocatable :: IE(:), JE(:), cell_type(:)
    real(8), allocatable :: solution(:,:)
    real(8), allocatable :: cell_solution(:,:)


    integer(4) LastChar
    integer(4) DOFs, DOFv
    integer, allocatable :: PointerDOFs(:), PointerDOFv(:)
    character (len=30) FieldName


    CHARACTER (LEN=21) :: ArchiveNameINPUT, ArchiveNameOUPUT


    integer(4) initial_count, final_count, step

    character (len=10) :: arg
    integer(4) Narg
    integer(4) Nlist
    integer(4), allocatable :: listDOF(:)

    !--------------------------------------------------------------------------------------------------------------------------------

    !------------------- INPUT ARGUMENTS -------------------------

    Narg = command_argument_count()

    if ( Narg==1 ) then
        call get_command_argument(1,arg)
        if ( (trim(Upper_Case(arg)) == '-H') .or. (trim(Upper_Case(arg)) == '--HELP') ) then
            E_IO = Help()   ! show help section
        else
            write(*,*) 'Inconsistent arguments'
            write(*,*) '### Exit with error ###'
            stop
        end if
        stop
    end if

    !----------------------- WELCOME ----------------------------
    E_IO = Welcome()
    !------------------------------------------------------------

    if (Narg==3) then

        call get_command_argument(1,arg)
        read(arg,'(I10)') initial_count

        call get_command_argument(2,arg)
        read(arg,'(I10)') final_count

        call get_command_argument(3,arg)
        read(arg,'(I10)') step

    else
        write(*,*) 'Inconsistent arguments'
        write(*,*) '### Exit with error ###'
        stop
    end if

    IF (.FALSE.) THEN   ! NO TERMINADO
        if (Narg>3) then    ! input arguments with -l or --list flag

            call get_command_argument(4,arg)
            if ( (trim(Upper_Case(arg)) == '-L') .or. (trim(Upper_Case(arg)) == '--LIST') ) then
                call get_command_argument(5,arg)
                read(arg,'(I10)') Nlist
                allocate(listDOF(Nlist))
                !ACA SE PUEDE SEGUIR TRABAJANDO CON ESTE METODO DE ENTRADA
                !ACA SE PUEDE SEGUIR TRABAJANDO CON ESTE METODO DE ENTRADA
                !ACA SE PUEDE SEGUIR TRABAJANDO CON ESTE METODO DE ENTRADA
                !ACA SE PUEDE SEGUIR TRABAJANDO CON ESTE METODO DE ENTRADA
                !NO ESTA TERMINADO
                !NO ESTA TERMINADO
                !NO ESTA TERMINADO
                !NO ESTA TERMINADO
                !NO ESTA TERMINADO
            end if


        end if

    END IF

    !initial_count = 0
    !final_count = 0
    !step = 1

    !-------------------





    Unit_MESH = GetUnit()
    Open(Unit_MESH, FILE = 'Mesh.txt')
    rewind(Unit_MESH)

    iError = iFindStringInFile('*NODAL DOFs',Unit_MESH)
    read(Unit_MESH,*) iDofT


    iError = iFindStringInFile('*DIMEN',Unit_MESH)
    read(Unit_MESH,*) Ndim

    iError = iFindStringInFile('*COORDINATES',Unit_MESH)
    read(Unit_MESH,*) NodT

    Allocate(x_uns(NodT),  &
        y_uns(NodT),  &
        z_uns(NodT)    )

    select case (Ndim)
        case(2)
            read(Unit_MESH,*) (x_uns(i), y_uns(i), i=1,NodT)
            z_uns = 0.d0
        case(3)
            read(Unit_MESH,*) (x_uns(i), y_uns(i), z_uns(i), i=1,NodT)
    end select

    iError = iFindStringInFile('*ELEMENT GROUPS',Unit_MESH)
    read(Unit_MESH,*) NGROUPS

    allocate(IDGroup(NGROUPS),  &
        NelTG(NGROUPS),    &
        NelName(NGROUPS),  &
        NodElG(NGROUPS)      )

    read(Unit_MESH,*) (IDGroup(i), NelTG(i), NelName(i), i=1,NGROUPS)

    allocate(IE(sum(NelTG)))

    If ( Upper_Case( NelName(1)(1:7) ) == 'GENERIC' ) then
        read(Unit_MESH,*) IE
    else
        write(*,*) 'ELEMENT TYPE NOT IMPLEMENTED'
    end if

    allocate(JE(sum(IE)))
    iError = iFindStringInFile('*INCIDENCE',Unit_MESH)
    read(Unit_MESH,*) JE
    JE = JE - 1   ! connectivity in VTK format include zero in numbering
                  ! so numbering is modidified

    close(Unit_MESH)

    !--------------------------------------------------------------------------------------------------------------------------------
    !##### this block generates connect array used by LIB
    !##### from IE and JE arrays used by FRAMEWORK. IE is
    !##### used before be modified by IEArrange function
    allocate(connect(size(IE) + size(JE)))
    n1=1
    n2=1
    do i=1, size(IE)
        connect(n1) = IE(i)
        n1=n1+1
        do j=n2, n2+IE(i)-1
            connect(n1) = JE(j)
            n1=n1+1
        enddo
        n2=j
    end do
    !--------------------------------------------------------------------------------------------------------------------------------



    !--------------------------------------------------------------------------------------------------------------------------------
    !# This block generates the cell_type array used by Library in VTK format

    allocate(cell_type(sum(NelTG)))
    cell_type = 0

    select case (Ndim)

        case (2)   ! domain elements in 2d problems
            do i=1, size(IE)
                select case (IE(i))
                    case(3)
                        cell_type(i) = 5    ! VTK_TRIANGLE
                    case(4)
                        cell_type(i) = 9    ! VTK_QUAD
                    case(6)
                        cell_type(i) = 22   ! VTK_QUADRATIC_TRIANGLE
                    case(8)
                        cell_type(i) = 23   ! VTK_QUADRATIC_QUAD
                    case default
                        write(*,*) 'Section: cell_type array generation'
                        write(*,*) 'BOUNDARY ELEMENTS DETECTED'
                        exit
                end select
            end do

        case (3)   ! domain elements in 3d problems
            do i=1, size(IE)
                select case (IE(i))
                    case(4)
                        cell_type(i) = 10   ! VTK_TETRA
                    case(8)
                        cell_type(i) = 12   ! VTK_HEXAHEDRON
                    case(10)
                        cell_type(i) = 24   ! VTK_QUADRATIC_TETRA
                    case(20)
                        cell_type(i) = 25   ! VTK_QUADRATIC_HEXAHEDRON
                    case default
                        write(*,*) 'Section: cell_type array generation'
                        write(*,*) 'BOUNDARY ELEMENTS DETECTED'
                        exit
                end select
            end do
    end select

    !--------------------------------------------------------------------------------------------------------------------------------

    !-----------------------------------------------------


    allocate(DOFNames(iDofT), PointerDOFs(iDofT), PointerDOFv(iDofT))

    Unit_BASPARAM = GetUnit()
    open(Unit_BASPARAM, FILE='Basparam.txt')

    iError = iFindStringInFile('*StepContinuationControl',Unit_BASPARAM)
    read(Unit_BASPARAM,*)
    read(Unit_BASPARAM,*)
    read(Unit_BASPARAM,*)
    read(Unit_BASPARAM,*) ( DOFNames(i) , i=1 , iDofT )


    ! busco grados de libertad vectoriales y escalares
    DOFv = 0
    DOFs = 0
    j = 0
    k = 0
    do i=1, iDofT

        if (DOFNames(i)(1:1) == '!') cycle

        LastChar = len(trim(DOFNAMES(i)))  ! DOF Name Lenght
        !    if (.true. .or. .false.) then
        if ( Upper_Case(DOFNAMES(i)(LastChar:LastChar)) == 'X' .or. DOFNames(i)(LastChar:LastChar) == '1' .or. &
            Upper_Case(DOFNAMES(i)(LastChar:LastChar)) == 'Y' .or. DOFNames(i)(LastChar:LastChar) == '2' .or. &
            Upper_Case(DOFNAMES(i)(LastChar:LastChar)) == 'Z' .or. DOFNames(i)(LastChar:LastChar) == '3' ) then

            DOFv = DOFv + 1
            j = j + 1
            PointerDOFv(j) = i
        else
            DOFs = DOFs + 1
            k = k + 1
            PointerDOFs(k) = i
        end if
    end do


    !----------------------------------------------------


    allocate(solution(NodT,iDofT))
    Unit_DataOUT = GetUnit()

    do i=initial_count, final_count, step


        write(ArchiveNameINPUT,'(1A9,1I8.8,1A4)')"DataOutN-",i,".txt"
        write(ArchiveNameOUPUT,'(1A9,1I8.8,1A4)')"ParaOutN-",i,".vtk"
        open(Unit_DataOUT, FILE = ArchiveNameINPUT)

        iError = iFindStringInFile('*Time',Unit_DataOUT)
        read(Unit_DataOUT,*)
        read(Unit_DataOUT,*) (solution(j,:), j=1, NodT)

        close(Unit_DataOUT)


        E_IO = VTK_INI(output_format = 'ASCII',                     &
            filename      = ArchiveNameOUPUT,            &
            title         = 'CubeTest',                  &
            mesh_topology = 'UNSTRUCTURED_GRID')


        E_IO = VTK_GEO_UNST_R8(NN = NodT, &
            X=x_uns,   &
            Y=y_uns,   &
            Z=z_uns)


        E_IO = VTK_CON(NC        = sum(NelTG),      &
            connect   = connect,         &
            cell_type = cell_type)


        E_IO = VTK_DAT(NC_NN        = NodT, &
            var_location = 'node')

        do j=1, DOFs

            FieldName = trim(DOFNAMES(PointerDOFs(j)))
            E_IO = VTK_VAR_SCAL_R8(NC_NN   = NodT, &
                varname = FieldName,            &
                var     = solution(:,PointerDOFs(j)))

        end do

        do j=1, DOFv, Ndim

            LastChar = len(trim(DOFNAMES(PointerDOFv(j))))
            FieldName = trim(DOFNAMES(PointerDOFv(j)))
            select case(Ndim)

                case(2)

                    E_IO = VTK_VAR_VECT_R8(NC_NN   = NodT,                              &
                        varname = FieldName(1:LastChar-1),           &
                        vec_type = 'vect',                           &
                        varX=solution(:,PointerDOFv(j)),             &
                        varY=solution(:,PointerDOFv(j)+1),           &
                        varZ=(/(0.0d0,k=1,NodT)/))

                case(3)

                    E_IO = VTK_VAR_VECT_R8(NC_NN   = NodT,                              &
                        varname = FieldName(1:LastChar-1),           &
                        vec_type = 'vect',                           &
                        varX=solution(:,PointerDOFv(j))  ,           &
                        varY=solution(:,PointerDOFv(j)+1),           &
                        varZ=solution(:,PointerDOFv(j)+2) )

            end select


        end do





        E_IO = VTK_END()

    end do

    deallocate(solution)
    deallocate(cell_type)

    write(*,*) 'Execution Completed'

CONTAINS
    function Welcome() result(E_IO)
    integer(4) E_IO

    write(*,*) '                #     #                                                  '
    write(*,*) '#     #         #     #            #######         ##   ## ####### #    #'
    write(*,*) '#     #               #               #             #   #     #    #   # '
    write(*,*) '#     # # ### ###   ######  #####     #     #####   #   #     #    #  #  '
    write(*,*) '#  #  # ##      #     #    #     #    #    #     #   # #      #    ###   '
    write(*,*) '# # # # #       #     #    #######    #    #     #   # #      #    #  #  '
    write(*,*) '##   ## #       #     #    #          #    #     #    #       #    #   # '
    write(*,*) '#     # #     #####    ###  #####     #     #####     #       #    #    #'
    write(*,*)

    Write(*,*) 'Version 2.1'
    Write(*,*) 'Created by NB'
    write(*,*) 'Input -h or --help for more details'
    write(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++'

    end function

    function Help() result(E_IO)
        !------------------------------------------------------------------
        !
        ! Write's manual page on screen
        !
        !------------------------------------------------------------------
        integer(4) E_IO

        !Welcome Message
        write(*,*) '                #     #                                                  '
        write(*,*) '#     #         #     #            #######         ##   ## ####### #    #'
        write(*,*) '#     #               #               #             #   #     #    #   # '
        write(*,*) '#     # # ### ###   ######  #####     #     #####   #   #     #    #  #  '
        write(*,*) '#  #  # ##      #     #    #     #    #    #     #   # #      #    ###   '
        write(*,*) '# # # # #       #     #    #######    #    #     #   # #      #    #  #  '
        write(*,*) '##   ## #       #     #    #          #    #     #    #       #    #   # '
        write(*,*) '#     # #     #####    ###  #####     #     #####     #       #    #    #'
        write(*,*)

        write(*,*) '### How to call it'
        write(*,*) '$: WriteToVTK $initial_step $final_step $step'
        write(*,*) '-------------------------------------------------------'
        write(*,*)
        write(*,*) 'DOF names are read from *StepContinuationControl flag in   '
        write(*,*) 'Basparam file.'
        write(*,*) 'Vectorial DOF are detected with x,y,z or 1,2,3 at the end DOF name'
        write(*,*) 'Plot all DOF by default. To plot in selective mode use "!"  '
        write(*,*) 'character at the begin of DOF that you want to ignore '


    !write(*,*) 'Basicamente hay tres formas de llamarlo:'
    !write(*,*) '### 1'
    !write(*,*) '$ WriteToVTK initial_step final_step step'
    !write(*,*) 'Grafica todos los DOF de acuerdo con a la definicion en '
    !write(*,*) 'Basparam en el flag *StepContinuationControl'
    !write(*,*)
    !write(*,*) '-------------------------------------------------------'
    !write(*,*) '### 2'
    !write(*,*) '$ WriteToVTK initial_step final_step step -l or --list ndof [list]'
    !write(*,*) ''

    end function Help


end program ImplementationVTK
