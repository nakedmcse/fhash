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

        function itoa(i) result(res)
            integer, intent(in) :: i
            character(:), allocatable :: res
            character(range(i)+2) :: tmp

            write(tmp, '(I0)') i
            res = trim(tmp)
        end function itoa

        function unwrap_int(value) result (res)
            integer :: res
            class(*) :: value
            select type(v => value)
            type is (integer)
                res = v
            class default
                res = -1
            end select
        end function unwrap_int

        function unwrap_str(value) result (res)
            character(len=:), allocatable :: res
            class(*) :: value
            select type(v => value)
            type is (character(*))
                res = v
            class default
                res = "unknown"
            end select
        end function unwrap_str

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