! ==============================================================================
! NASA/GSFC, CISTO, Code 606, Advanced Software Technology Group
! ==============================================================================
!
! MODULE: TODO_module_name
!
! AUTHOR:
! TODO_name_and_affiliation
!
! DESCRIPTION:
! TODO_describe_purpose_of_module.
! ==============================================================================
MODULE mod_strings

    ! --------------------------------------------------------------------------
    IMPLICIT NONE
    PRIVATE ! NOTE_avoid_public_variables_if_possible
    ! --------------------------------------------------------------------------
    PUBLIC :: FindStringInFile             ! TODO_optional_brief_description
    ! --------------------------------------------------------------------------

! ==============================================================================
CONTAINS
! ==============================================================================

    ! ================================================================================
    FUNCTION FindStringInFile(Str, ioUnit, Mandatory) RESULT (iError)
        ! ================================================================================
        ! Busca un String en un archivo (STR), sino lo encuentra rebovina el archivo
        ! y pone iError < 0 como indicador de no hallazgo
        ! Str: String to find, ioUnit: Unit assigned to Input File; iError: Error Status variable
        ! ================================================================================
        IMPLICIT NONE
        ! ================================================================================
        ! Parameters
        LOGICAL,PARAMETER  :: Segui=.True.
        ! ================================================================================
        ! Arguments
        CHARACTER(*), INTENT(IN) :: Str
        INTEGER, INTENT (IN) :: ioUnit
        LOGICAL, OPTIONAL, INTENT(IN) :: Mandatory
        ! ================================================================================
        ! Locals
        LOGICAL :: MandatoryL
        CHARACTER(LEN=120) :: DummyString
        CHARACTER(LEN=120) :: upStr
        INTEGER :: iError
        INTEGER :: Leng
        ! ================================================================================

        ! ================================================================================
        IF ( PRESENT(Mandatory) ) THEN
            MandatoryL = Mandatory
        ELSE
            MandatoryL = .FALSE.
        END IF
        ! ================================================================================
        iError=0
        Leng = LEN_TRIM(Str)
        upStr = Upper_Case(Str)       ! Line added by NB. Converts Str to Upper Case string
        ! ================================================================================
        REWIND(ioUnit)
        Search_Loop: DO WHILE (segui)
            READ(ioUnit,'(1A120)',IOSTAT=iError) DummyString
            DummyString = Upper_Case(DummyString)   ! line added by NB
            !       if (iError==59) backspace(ioUnit)
            IF (iError.lt.0) THEN
                REWIND (ioUnit)
                EXIT Search_Loop
            END IF
            IF ( DummyString(1:1)    /=    '*'      ) CYCLE Search_Loop
            IF ( DummyString(1:Leng) == upStr(1:Leng) ) EXIT Search_Loop
        END DO Search_Loop
        ! ================================================================================
        IF (MandatoryL) THEN
            IF ( .NOT. ( iError == 0 ) ) THEN
                WRITE(*,*) upStr, 'NOT FOUND'
                STOP
            END IF
        END IF
        ! ================================================================================

    END FUNCTION FindStringInFile
    ! ================================================================================



    ! ================================================================================
    FUNCTION Upper_Case(string)

        !-------------------------------------------------------------------------------------------
        !! The Upper_Case function converts the lower case characters of a string to upper case one.
        !! Use this function in order to achieve case-insensitive: all character variables used
        !! are pre-processed by Uppper_Case function before these variables are used. So the users
        !! can call functions without pey attention of case of the keywords passed to the functions.
        !-------------------------------------------------------------------------------------------

        IMPLICIT NONE

        !-------------------------------------------------------------------------------------------
        CHARACTER(LEN=*), INTENT(IN)    :: string     ! string to be converted
        CHARACTER(LEN=LEN(string))      :: Upper_Case ! converted string
        INTEGER                         :: n1         ! characters counter
        !-------------------------------------------------------------------------------------------

        Upper_Case = string
        DO n1=1,LEN(string)
            SELECT CASE(ICHAR(string(n1:n1)))
                CASE(97:122)
                    Upper_Case(n1:n1)=CHAR(ICHAR(string(n1:n1))-32) ! Upper case conversion
            END SELECT
        ENDDO
        RETURN

        !-----------------------------------------------------------------------------------------------
    END FUNCTION Upper_Case
    ! ================================================================================


END MODULE mod_strings
! ==============================================================================
