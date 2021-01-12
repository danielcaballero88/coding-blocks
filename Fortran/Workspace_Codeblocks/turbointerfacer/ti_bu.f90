!Turbo Interfacer
!brought to you by Daniel Enrique Caballero
!copied from SAU's ArmaMesh and modified by dancab
!this code reads and modifies a mesh in BuildMesh.txt format (Venere format?)
!
!in the case that a mesh is given with a surface between a solid and a fluid, for FSI
!models, it is necessary to couple the two domains at the shared surface.
!to do so, this application does that in the following way:
!   - duplicates the nodes in the shared surface
!   - changes the incidence of one of the two domains (changes the surface nodes for the duplicated ones)
!   - creates coupling elements (prisms) between the original triangles and the duplicated ones
!in order to acomplish that, some information must be given in a TurboInterfacerInfo.txt file:
!
!Archivo de malla (tipicamente BuildMesh.txt)
!Ndim
!Numero de grupos de superficie con nodos a duplicar para crear los prismas
!Id. de cada grupo de los anteriores
!Numero de grupos con incidencia modificada
!Id. de los grupos modificados
PROGRAM turbointerfacer

    IMPLICIT REAL*8 (A-H,O-Z)
    Character DummyString*120, Str*120, NelName*12,MeshFileName*60
    CHARACTER(LEN=120) :: PhysName

    REAL(8), ALLOCATABLE :: X(:) !coordinates
    INTEGER, ALLOCATABLE :: JE(:) !connectivity: list of nodes
    INTEGER, ALLOCATABLE :: IE(:) !connectivity: where in JE starts each element
    INTEGER, ALLOCATABLE :: NelTG(:) !number of elements in <index> group
    ALLOCATABLE          :: NelName(:) !name of elements in <index> group (array of strings)
    ALLOCATABLE          :: PhysName(:) !name of the <index> physical group  (array of strings)
    INTEGER, ALLOCATABLE :: Nodel(:) !number of nodes per element in <index> group
    INTEGER, ALLOCATABLE :: ipNelG(:) !indicates where in IE starts each group of elements
    INTEGER, ALLOCATABLE :: ipJEG(:) !indicates where in JE starts each group of elements
    INTEGER, ALLOCATABLE :: IdNodeInt(:) !array of size (nodt) used to mark up the interface nodes and duplicate them
    LOGICAL              :: flag_physical_groups !flag to check if in the inputmeshfile there are or not physical groups

    Allocatable idFSIgroups(:)
    Allocatable iGroupsChangeIncidence(:)
    Allocatable isFSIgroup(:)


    Integer*4 OutMeshFile,FindStringInFile,InfoFile
    Real*8 XA(3),XB(3),Vn(3),ScaleF(3)

    CHARACTER(LEN=*), PARAMETER :: FMT100 = '("(",I0,"G22.14)")'
    CHARACTER(LEN=*), PARAMETER :: FMT101 = '("(",I0,"I8)")'
    CHARACTER(LEN=*), PARAMETER :: FMT200 = '("(1A",I0,")")'
    CHARACTER(LEN=*), PARAMETER :: FMT300 = '("(1I11,",I0,"G22.14)")'
    CHARACTER(LEN=50) :: formato

    WRITE(*,*) "  _______         _             _____       _             __                      "
    WRITE(*,*) " |__   __|       | |           |_   _|     | |           / _|                     "
    WRITE(*,*) "    | |_   _ _ __| |__   ___     | |  _ __ | |_ ___ _ __| |_ __ _  ___ ___ _ __   "
    WRITE(*,*) "    | | | | | '__| '_ \ / _ \    | | | '_ \| __/ _ \ '__|  _/ _` |/ __/ _ \ '__|  "
    WRITE(*,*) "    | | |_| | |  | |_) | (_) |  _| |_| | | | ||  __/ |  | || (_| | (_|  __/ |     "
    WRITE(*,*) "    |_|\__,_|_|  |_.__/ \___/  |_____|_| |_|\__\___|_|  |_| \__,_|\___\___|_|     "
    WRITE(*,*) "                                                                                  "
    WRITE(*,*) " Brought to you by dancab!"


    MeshFile  = 13
    OutMeshFile = 14
    InfoFile = 15

    OPEN ( InfoFile, FILE= 'TurboInterfacerInfo.txt' )
    READ(InfoFile,"(30A)") MeshFileName
    Read (InfoFile,*) Ndim

    OPEN ( MeshFile,  FILE= MeshFileName )
    OPEN ( OutMeshFile, FILE= 'BuildMesh2.txt' )
    rewind(OutMeshFile)

    Write(*,*)"Processing files"

    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !Reading the mesh
    ! Reading coordinates
    Str='*COORDINATES'
    iError = FindStringInFile(Str,MeshFile)
    if (iError.ne.0) then
        Write(*,*)Str,"NOT FOUND"
        Stop
    End if
    read(MeshFile,*) NodT
    allocate(X(nDim*NodT))
    Do I=1,NodT
        read(MeshFile,*)N, (X( nDim*(I-1) + J ),J=1,nDim)
    Enddo

    ! Reading Physical Groups
    flag_physical_groups = .TRUE.
    rewind(MeshFile)
    Str='*PHYSICAL GROUPS'
    iError = FindStringInFile(Str,MeshFile)
    IF (iError.ne.0) THEN
        Write(*,*)Str,"NOT FOUND, this mesh format is not recommended"
        flag_physical_groups = .FALSE.
    ELSE
        read(MeshFile,*) NelG
        allocate(PhysName(NelG))
    END IF
    Do I=1,NelG
        read(MeshFile,*)iaux1, iaux2,PhysName(I)
    Enddo

    ! Reading Element Groups
    rewind(MeshFile)
    Str='*ELEMENT GROUPS'
    iError = FindStringInFile(Str,MeshFile)
    if (iError.ne.0) then
        Write(*,*)Str,"NOT FOUND"
        Stop
    End if
    read(MeshFile,*) NelG
    allocate(NelTG(NelG),NodEl(NelG),NelName(NelG))
    Do I=1,NelG
        read(MeshFile,*)N, NelTG(I),NelName(I)
    Enddo

    NelT=Sum(NelTG)
    allocate(IE(NelT+1))
    IE=0
    ipNel=0
    do ng=1,NelG
        Len= Len_Trim(NelName(ng))
        NodEl(ng)=0
        if ( NelName(ng)(1:Len) == "Line2"  ) NodEl(ng)=2   !     2-node line.
        if ( NelName(ng)(1:Len) == "Tri3"   ) NodEl(ng)=3   !     3-node triangle.
        if ( NelName(ng)(1:Len) == "Quad4"  ) NodEl(ng)=4   !     4-node quadrangle.
        if ( NelName(ng)(1:Len) == "Tetra4" ) NodEl(ng)=4   !     4-node tetrahedron.
        if ( NelName(ng)(1:Len) == "Hexa8"  ) NodEl(ng)=8   !     8-node hexahedron.
        if ( NelName(ng)(1:Len) == "Prism6" ) NodEl(ng)=6   !     6-node prism.
        if ( NelName(ng)(1:Len) == "Pyram5" ) NodEl(ng)=5   !     5-node pyramid.
        if ( NelName(ng)(1:Len) == "Line3"  ) NodEl(ng)=3   !     3-node second order line (2 nodes associated with the vertices and 1 with the edge).
        if ( NelName(ng)(1:Len) == "Tri6"   ) NodEl(ng)=6   !     6-node second order triangle (3 nodes associated with the vertices and 3 with the edges).
        if ( NelName(ng)(1:Len) == "Quad9"  ) NodEl(ng)=9   !     9-node second order quadrangle (4 nodes associated with the vertices, 4 with the edges and 1 with the face).
        if ( NelName(ng)(1:Len) == "Tetra10") NodEl(ng)=10  !    10-node second order tetrahedron (4 nodes associated with the vertices and 6 with the edges).
        if ( NelName(ng)(1:Len) == "Hexa27" ) NodEl(ng)=27  !    27-node second order hexahedron (8 nodes associated with the vertices, 12 with the edges, 6 with the faces and 1 with the volume).
        if ( NelName(ng)(1:Len) == "Prism18") NodEl(ng)=18  !    18-node second order prism (6 nodes associated with the vertices, 9 with the edges and 3 with the quadrangular faces).
        if ( NelName(ng)(1:Len) == "Pyram14") NodEl(ng)=14  !    14-node second order pyramid (5 nodes associated with the vertices, 8 with the edges and 1 with the quadrangular face).
        if ( NelName(ng)(1:Len) == "Point1" ) NodEl(ng)=1   !     1-node point.
        if ( NelName(ng)(1:Len) == "Quad8"  ) NodEl(ng)=8   !     8-node second order quadrangle (4 nodes associated with the vertices and 4 with the edges).
        if ( NelName(ng)(1:Len) == "Hexa20" ) NodEl(ng)=20  !    20-node second order hexahedron (8 nodes associated with the vertices and 12 with the edges).
        if ( NelName(ng)(1:Len) == "Prism15") NodEl(ng)=15  !    15-node second order prism (6 nodes associated with the vertices and 9 with the edges).
        if ( NelName(ng)(1:Len) == "Pyram14") NodEl(ng)=13  !    13-node second order pyramid (5 nodes associated with the vertices and 8 with the edges).
        if ( NelName(ng)(1:Len) == "Tri9"   ) NodEl(ng)=9   !     9-node third order incomplete triangle (3 nodes associated with the vertices, 6 with the edges)
        if ( NelName(ng)(1:Len) == "Tri10"  ) NodEl(ng)=10  !    10-node third order triangle (3 nodes associated with the vertices, 6 with the edges, 1 with the face)
        if ( NelName(ng)(1:Len) == "Tri12"  ) NodEl(ng)=12  !    12-node fourth order incomplete triangle (3 nodes associated with the vertices, 9 with the edges)
        if ( NelName(ng)(1:Len) == "Tri15"  ) NodEl(ng)=15  !    15-node fourth order triangle (3 nodes associated with the vertices, 9 with the edges, 3 with the face)
        if ( NelName(ng)(1:Len) == "Tri15i" ) NodEl(ng)=15  !    15-node fifth order incomplete triangle (3 nodes associated with the vertices, 12 with the edges)
        if ( NelName(ng)(1:Len) == "Tri21"  ) NodEl(ng)=21  !    21-node fifth order complete triangle (3 nodes associated with the vertices, 12 with the edges, 6 with the face)
        if ( NelName(ng)(1:Len) == "Edge4"  ) NodEl(ng)=4   !     4-node third order edge (2 nodes associated with the vertices, 2 internal to the edge)
        if ( NelName(ng)(1:Len) == "Edge5"  ) NodEl(ng)=5   !     5-node fourth order edge (2 nodes associated with the vertices, 3 internal to the edge)
        if ( NelName(ng)(1:Len) == "Edge6"  ) NodEl(ng)=6   !     6-node fifth order edge (2 nodes associated with the vertices, 4 internal to the edge)
        if ( NelName(ng)(1:Len) == "Tetra20") NodEl(ng)=20  !    20-node third order tetrahedron (4 nodes associated with the vertices, 12 with the edges, 4 with the faces)
        if ( NelName(ng)(1:Len) == "Tetra35") NodEl(ng)=35  !    35-node fourth order tetrahedron (4 nodes associated with the vertices, 18 with the edges, 12 with the faces, 1 in the volume)
        if ( NelName(ng)(1:Len) == "Tetra56") NodEl(ng)=56  !    56-node fifth order tetrahedron (4 nodes associated with the vertices, 24 with the edges, 24 with the faces, 4 in the volume)
        if ( NelName(ng)(1:Len) == "Hexa64" ) NodEl(ng)=64  !    64-node third order hexahedron (8 nodes associated with the vertices, 24 with the edges, 24 with the faces, 8 in the volume)
        if ( NelName(ng)(1:Len) == "Hexa125") NodEl(ng)=125 !   125-node fourth order hexahedron (8 nodes associated with the vertices, 36 with the edges, 54 with the faces, 27 in the volume)

        if (NodEl(ng).eq.0) then
            write(*,*)"ELEMENT TYPE NOT IMPLEMENTED"
            Stop
        end if
        do Nel=1,NelTG(ng)
            IE(ipNel+Nel)=NodEl(ng)
        end do
        ipNel=ipNel+NelTG(ng)
    end do
    rewind(MeshFile)

    LonJE=Sum(IE)
    Allocate( JE(LonJE) )
    Str='*INCIDENCE'
    iError = FindStringInFile(Str,MeshFile)
    if (iError.ne.0) then
        Write(*,*)Str,"NOT FOUND"
        Stop
    End if

    Read(MeshFile,*)JE

    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    ! END reading input Mesh
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

    ! Ubico donde empieza cada grupo de elementos en el IE y JE, los valores se almacenan en ipNelG(),ipJEG()
    Allocate( ipNelG(NelG),ipJEG(NelG) )
    Do ng = 1, NelG
        NgL   = ng
        ipNel = 0
        iPJE  = 0
        Do ngi = 1, NgL-1
            ipNel = ipNel + NelTG(ngi)
            iPJE  = iPJE + NelTG(ngi)*NodEl(ngi)
        End do
        ipNelG(ng) = ipNel
        ipJEG (ng) = iPJE
    Enddo

    !!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<
    !!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<
    !!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<
    !Reading InfoFile
    Read ( InfoFile,* )nFSIgroups !number of groups of interfaces to create
    Allocate( idFSIgroups(nFSIgroups) )
    Read ( InfoFile,* )idFSIgroups !id of the interfaces

    ALLOCATE ( isFSIgroup(NelG) )
    isFSIgroup = 0
    Do iGSup=1,nFSIgroups
        isFSIgroup( idFSIgroups(iGSup) )=1
    Enddo

    Read ( InfoFile,* )NGroupsChangeIncidence !number of domain groups to change incidence
    Allocate( iGroupsChangeIncidence(NGroupsChangeIncidence) )
    Read ( InfoFile,* )iGroupsChangeIncidence !id of those domain groups

    ! =================================
    ! Armo los nodos nuevos de interfaz
    ! =================================

    !LOOP para marcar los nodos que pertenecen a la superficie de interface
    Allocate( IdNodeInt( NodT ) )
    IdNodeInt=0
    Do iSup=1,nFSIgroups
        ng=idFSIgroups(iSup)
        Nodos  = NodEl(ng)  !NodEl tiene la cantidad de nodos por elemento
        Do Nel = 1, NelTG(ng) !NelTG cantidad de elementos del grupo ng
            iNa = iPJEG(ng) + (Nel-1)*Nodos + 1 !primer nodo del triangulo Nel en el JE
            iNb = iPJEG(ng) +  Nel   *Nodos     !ultimo nodo de Nel en el JE
            Do iN=iNa,iNb
                IdNodeInt( JE(iN) ) = -1        !marco todos los nodos del triangulo Nel con un -1
            End do
        Enddo
    Enddo

    !LOOP para indicar en el vector IdNodeInt los nuevos nodos
    !el indice indica el nodo original y el valor almacenado indica el nodo duplicado
    inodonew = 0
    Do nod = 1, NodT
        if( IdNodeInt( nod ) < 0 ) then !si encuentro
            inodonew = inodonew + 1
            IdNodeInt( nod ) = Nodt+inodonew
        endif
    Enddo

    !LOOP para corregir la conectividad de los grupos de dominio que lo necesiten
    Do iG = 1, NGroupsChangeIncidence
        ng  = iGroupsChangeIncidence(iG) !id de cada grupo a cambiar la conectividad
        NodElng = NodEl(ng) !numero de nodos por elemento en el grupo
        ii  = iPJEG(ng)     !lugar en el JE donde comienza el grupo
        !loop sobre todos los elementos del grupo
        do i=1, NelTG(ng)
            !loop sobre los nodos de cada elemento del grupo
            do J=1, NodElng
                nodo = JE(ii+j) !nodo J del elemento i
                nodo_dup = IdNodeInt( nodo)  !id de nodo duplicado (=0 si no fue duplicado)
                !si el nodo ha sido duplicado, lo modifico en la conectividad
                If ( ( nodo_dup )  > 0 ) JE(ii+j)=nodo_dup
            enddo
            ii= ii+NodElng
        enddo
    End Do
    !!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<
    !!!!>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>><<<<<<<<<<<<<<<<<<<<


    !<--------------------------------------------------->
    !              SALIDA DE DATOS
    !<--------------------------------------------------->
    Write(6,*)"#########################################"
    Write(6,*)"      Procesing Mesh.txt"
    Write(6,*)"#########################################"

    !Escribo los Elementos
    IF (flag_physical_groups) THEN
            Str='*PHYSICAL GROUPS'
        inw = Len_Trim(Str)
        WRITE(formato,FMT200) inw
        WRITE(OutMeshFile,formato)Str
        WRITE(OutMeshFile,'(1I7)')NelG
        Do I=1,NelG
                WRITE(OutMeshFile,'(1I7,1x,1I7,1x,1A50)')I, NelTG(I),TRIM(PhysName(I))
        Enddo
    END IF

    Str='*ELEMENT GROUPS'
    inw = Len_Trim(Str)
    WRITE(formato,FMT200) inw
    WRITE(OutMeshFile,formato)Str
    WRITE(OutMeshFile,'(1I7)')NelG
    Do I=1,NelG
        !si el grupo es un grupo de interfaz, escribo los elementos de acople solamente
        if( isFSIgroup( I ) .NE. 0) then
            WRITE(OutMeshFile,'(1I7,1x,1I7,1x,1A10)')I, NelTG(I)," Prism6"
        else
            WRITE(OutMeshFile,'(1I7,1x,1I7,1x,1A10)')I, NelTG(I),NelName(I)
        endif
    Enddo

    !%%%%%%%%%%
    !Salida JE
    !%%%%%%%%%%
    Str='*INCIDENCE'
    inw = Len_Trim(Str)
    WRITE(formato,FMT200) inw
    WRITE(OutMeshFile,formato)Str

    Do ng=1,NelG
        !si el grupo es de interfaz, escribo los elementos de acople
        if( isFSIgroup(ng).NE.0) then

            ii       = iPJEG(ng)
            NodElOld = NodEl(ng)
            inw      = 2*NodElOld
            do i=1, NelTG(ng)
                WRITE (OutMeshFile,'(10I7)')(JE(ii+j),j=1,NodElOld),(IdNodeInt(JE(ii+jj)),jj=1,NodElOld)
                ii= ii+NodElOld
            enddo
            WRITE (OutMeshFile,*)

        else

            inw = NodEl(ng)
            ii  = iPJEG(ng)
            do i=1, NelTG(ng)
                WRITE (OutMeshFile,'(10I7)')(JE(ii+j),j=1,inw)
                ii= ii+inw
            enddo
            WRITE (OutMeshFile,*)

        endif

    Enddo

    !Escribo las coordenadas nodales
    Str='*COORDINATES'
    inw = Len_Trim(Str)
    WRITE(formato,FMT200) inw
    WRITE(OutMeshFile,formato)Str
    WRITE (OutMeshFile,'(1I7)')NodT+inodonew

    !nDimgmsh = 3
    do i=1, NodT
        WRITE (OutMeshFile,'(1I7,1x,3E15.6)') I,(X(Ndim*(I-1)+J),J=1,3) ! 3=Ndim
    enddo
    do I=1, NodT
        if( IdNodeInt( I ) > 0 ) then
            WRITE (OutMeshFile,'(1I7,1x,3E15.6)') IdNodeInt( I ),(X(Ndim*(I-1)+J),J=1,3) ! 3=Ndim
        endif
    enddo

    Close (OutMeshFile)
    WRITE(*,*) "END OF PROGRAM"
END PROGRAM

!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Integer*4 Function FindStringInFile(Str,ioUnit)
    ! Busca un String en un archivo (STR), sino lo encuentra rebovina el archivo
    ! y pone iError < 0 como indicador de no hallazgo
    ! Str: String to find, ioUnit: Unit assigned to Input File; iError: Error Status variable
    Logical,Parameter  :: Segui=.True.
    CHARACTER(LEN=*), PARAMETER :: formato = "(1A120)"
    Character(*) Str,DummyString*120

    iError=0
    Leng = Len_Trim(Str)
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    Search_Loop: do while (segui)
        read(ioUnit,formato,iostat=iError)DummyString
        !       if (iError==59) backspace(ioUnit)
        if (iError.lt.0) then
            rewind (ioUnit)
            exit Search_Loop
        endif
        if ( DummyString(1:1)    /=    '*'      ) Cycle Search_Loop
        if ( DummyString(1:Leng) == Str(1:Leng) ) Exit Search_Loop
    end do Search_Loop
    !>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
    FindStringInFile = iError
    Return
End Function

