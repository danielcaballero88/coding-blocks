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
MODULE mod_io

    ! --------------------------------------------------------------------------
    IMPLICIT NONE
    PRIVATE ! NOTE_avoid_public_variables_if_possible
    ! --------------------------------------------------------------------------
    PUBLIC :: someFunction             ! TODO_optional_brief_description
    ! --------------------------------------------------------------------------

! ==============================================================================
CONTAINS
! ==============================================================================

    ! ==========================================================================
    FUNCTION someFunction(inParam, outParam, inOutParam) RESULT(returnValue)
        ! ----------------------------------------------------------------------
        ! DESCRIPTION:
        ! TODO_describe_routine_purpose.
        ! ----------------------------------------------------------------------
        IMPLICIT NONE
        ! ----------------------------------------------------------------------
        REAL(8), INTENT(IN) :: inParam         ! TODO_description
        REAL(8), INTENT(OUT) :: outParam       ! TODO_description
        REAL(8), INTENT(INOUT) :: inOutParam   ! TODO_description
        ! ----------------------------------------------------------------------
        REAL(8) :: returnValue                 ! TODO_description
        ! ----------------------------------------------------------------------
        REAL(8) :: someVariable                ! TODO_description
        ! ----------------------------------------------------------------------

        ! ______________________________________________________________________
        ! TODO_insert_code_here
        ! ______________________________________________________________________

        ! ----------------------------------------------------------------------
    END FUNCTION someFunction
    ! ==========================================================================


END MODULE mod_io
! ==============================================================================
