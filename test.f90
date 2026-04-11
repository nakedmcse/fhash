! Fortran Hash Table and Array tests
program test
    use fhash
    implicit none

    ! Tests
    call test_array_ints()
    call test_array_strings()
    call test_hash_table_ints()
    call test_hash_table_strings()

    contains
        subroutine assert(condition, message)
            logical :: condition
            character(len=*) :: message
            if (.not.(condition)) then
                print *, "Assertion failed: ", message
                error stop
            end if
        end subroutine assert

        subroutine test_array_ints()
            ! Given
            ! When
            ! Then
        end subroutine test_array_ints

        subroutine test_array_strings()
            ! Given
            ! When
            ! Then
        end subroutine test_array_strings

        subroutine test_hash_table_ints()
            ! Given
            ! When
            ! Then
        end subroutine test_hash_table_ints

        subroutine test_hash_table_strings()
            ! Given
            ! When
            ! Then
        end subroutine test_hash_table_strings
end program test