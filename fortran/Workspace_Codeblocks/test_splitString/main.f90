SUBROUTINE split_string(instring, string1, string2, delimchar)
    !split a string into 2 either side of a delimiter token
    IMPLICIT NONE
    CHARACTER(LEN=*), INTENT(IN)   :: instring
    CHARACTER(LEN=*), INTENT(OUT)  :: string1,string2
    CHARACTER   :: delimchar
    INTEGER     :: length
    INTEGER     :: i

    length  = LEN_TRIM(instring)

    i = SCAN(instring, delimchar)
    string1 = instring(1:i-1)
    string2 = instring(i+1:length)

END SUBROUTINE split_string


program hello
    implicit none
    CHARACTER(120) :: longString
    CHARACTER(120) :: str1, str2
    INTEGER :: lentri
    INTEGER :: i

    longString = 'GPsolver ; MD.txt'
    lentri = LEN_TRIM(longString)
    WRITE(*,*) 'lentri= ', lentri

    CALL split_string(longString, str1, str2, ':')

    WRITE(*,*) LEN(str1), str1
    WRITE(*,*) LEN(str2), str2


    print *, "Hello World!"

end program

