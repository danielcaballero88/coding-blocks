PROGRAM main
    CHARACTER(LEN=60) :: FSIMeshName = "BuildMesh.txt", InMeshName = "BuildMesh.txt", InfoFileName = "BuildInfo.txt"

    OPEN(10, FILE=FSIMeshName) !this will be the inmeshfile for ArmaMesh but the OutMeshFile for TurboInterfacer

    CALL turbointerfacer(10, 3)

    WRITE(*,*) "END OF PROGRAM"


END PROGRAM
