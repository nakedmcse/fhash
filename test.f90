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
            type(fhash_array) :: array
            type(fhash_kv) :: item, popped
            integer :: i
            ! When
            do i = 1, 513
                item%key = itoa(i)
                item%value = i
                call array%append(item)
            end do
            call array%pop(popped)
            ! Then
            call assert(array%count == 512, "Int array count is wrong")
            call assert(unwrap_int(array%items(1)%value) == 1, "Int array first element is wrong")
            call assert(unwrap_int(array%items(256)%value) == 256, "Int array middle element is wrong")
            call assert(unwrap_int(array%items(array%count)%value) == 512, "Int array last element is wrong")
            call assert(unwrap_int(popped%value) == 513, "Int array popped element is wrong")
            print *,"Int array test passed"
        end subroutine test_array_ints

        subroutine test_array_strings()
            ! Given
            type(fhash_array) :: array
            type(fhash_kv) :: item, popped
            integer :: i
            ! When
            do i = 1, 513
                item%key = itoa(i)
                item%value = "String " // itoa(i)
                call array%append(item)
            end do
            call array%pop(popped)
            ! Then
            call assert(array%count == 512, "String array count is wrong")
            call assert(unwrap_str(array%items(1)%value) == "String 1", "String array first element is wrong")
            call assert(unwrap_str(array%items(256)%value) == "String 256", "String array middle element is wrong")
            call assert(unwrap_str(array%items(array%count)%value) == "String 512", "String array last element is wrong")
            call assert(unwrap_str(popped%value) == "String 513", "String array popped element is wrong")
            print *,"String array test passed"
        end subroutine test_array_strings

        subroutine test_hash_table_ints()
            ! Given
            type(fhash_ht) :: hashtable
            type(fhash_kv) :: item, popped
            integer :: i
            ! When
            call hashtable%init(256)
            do i = 1, 513
                item%key = itoa(i)
                item%value = i
                call hashtable%set(item)
            end do
            ! update item
            item%key = "256"
            item%value = 514
            call hashtable%set(item)
            ! Then
            call assert(hashtable%storage%count == 513, "Int hashtable count is wrong")
            call hashtable%get("1",item)
            call assert(unwrap_int(item%value) == 1, "Int hashtable element with key 1 is wrong")
            call hashtable%get("256",item)
            call assert(unwrap_int(item%value) == 514, "Int hashtable element with key 256 is wrong")
            call hashtable%get("513",item)
            call assert(unwrap_int(item%value) == 513, "Int hashtable element with key 513 is wrong")
            print *,"Int hashtable test passed"
        end subroutine test_hash_table_ints

        subroutine test_hash_table_strings()
            ! Given
            type(fhash_ht) :: hashtable
            type(fhash_kv) :: item, popped
            integer :: i
            ! When
            call hashtable%init(256)
            do i = 1, 513
                item%key = itoa(i)
                item%value = "String " // itoa(i)
                call hashtable%set(item)
            end do
            ! update item
            item%key = "256"
            item%value = "String 514"
            call hashtable%set(item)
            ! Then
            call assert(hashtable%storage%count == 513, "String hashtable count is wrong")
            call hashtable%get("1",item)
            call assert(unwrap_str(item%value) == "String 1", "String hashtable element with key 1 is wrong")
            call hashtable%get("256",item)
            call assert(unwrap_str(item%value) == "String 514", "String hashtable element with key 256 is wrong")
            call hashtable%get("513",item)
            call assert(unwrap_str(item%value) == "String 513", "String hashtable element with key 513 is wrong")
            print *,"String hashtable test passed"
        end subroutine test_hash_table_strings
end program test