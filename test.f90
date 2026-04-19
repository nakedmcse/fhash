! Fortran Hash Table and Array tests
program test
    use fhash
    implicit none

    ! Tests
    call test_array_ints()
    call test_array_strings()
    call test_hash_table_ints()
    call test_hash_table_strings()
    call test_list_ints()
    call test_list_strings()

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
            ! remove item
            call hashtable%remove("254")
            ! Then
            call assert(hashtable%storage%count == 513, "Int hashtable count is wrong")
            call hashtable%get("1",item)
            call assert(unwrap_int(item%value) == 1, "Int hashtable element with key 1 is wrong")
            call hashtable%get("256",item)
            call assert(unwrap_int(item%value) == 514, "Int hashtable element with key 256 is wrong")
            call hashtable%get("254",item)
            call assert(item%error, "Int hashtable element with key 254 not removed")
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
            ! remove item
            call hashtable%remove("254")
            ! Then
            call assert(hashtable%storage%count == 513, "String hashtable count is wrong")
            call hashtable%get("1",item)
            call assert(unwrap_str(item%value) == "String 1", "String hashtable element with key 1 is wrong")
            call hashtable%get("256",item)
            call assert(unwrap_str(item%value) == "String 514", "String hashtable element with key 256 is wrong")
            call hashtable%get("254",item)
            call assert(item%error, "String hashtable element with key 254 not removed")
            call hashtable%get("513",item)
            call assert(unwrap_str(item%value) == "String 513", "String hashtable element with key 513 is wrong")
            print *,"String hashtable test passed"
        end subroutine test_hash_table_strings

        subroutine test_list_ints()
            ! Given
            type(fhash_list) :: list
            type(fhash_list_node) :: item, popped, shifted
            type(fhash_list_node), pointer :: get_index, get_key, get_error
            integer :: i
            ! When
            do i = 1,10
                item%key = itoa(i)
                item%value = i
                call list%append_node(item)
            end do
            call list%pop_node(popped)
            call list%shift_node(shifted)
            call list%get_node(get_index,2)
            call list%get_node(get_key,"6")
            call list%remove_node("7")
            call list%get_node(get_error,"7")
            ! Then
            call assert(list%count == 7, "Int list count wrong")
            call assert(unwrap_int(popped%value) == 10, "Int list popped value wrong")
            call assert(unwrap_int(list%footer%value) == 9, "Int list tail peek value wrong")
            call assert(unwrap_int(shifted%value) == 1, "Int list shifted value wrong")
            call assert(unwrap_int(list%header%value) == 2, "Int list head peek value wrong")
            call assert(unwrap_int(get_index%value) == 3, "Int list get index value wrong")
            call assert(.not. get_index%error, "Int list get index error set")
            call assert(unwrap_int(get_key%value) == 6, "Int list get key value wrong")
            call assert(.not. get_key%error, "Int list get key error set")
            call assert(.not. associated(get_error), "Int list get error was associated")
            print *,"Int list test passed"
        end subroutine test_list_ints

        subroutine test_list_strings()
            ! Given
            type(fhash_list) :: list
            type(fhash_list_node) :: item, popped, shifted
            type(fhash_list_node), pointer :: get_index, get_key, get_error
            integer :: i
            ! When
            do i = 1,10
                item%key = itoa(i)
                item%value = "String " // itoa(i)
                call list%append_node(item)
            end do
            call list%pop_node(popped)
            call list%shift_node(shifted)
            call list%get_node(get_index,2)
            call list%get_node(get_key,"6")
            call list%remove_node("5")
            call list%get_node(get_error,"5")
            ! Then
            call assert(list%count == 7, "String list count wrong")
            call assert(unwrap_str(popped%value) == "String 10", "String list popped value wrong")
            call assert(unwrap_str(list%footer%value) == "String 9", "String list tail peek value wrong")
            call assert(unwrap_str(shifted%value) == "String 1", "String list shifted value wrong")
            call assert(unwrap_str(list%header%value) == "String 2", "String list head peek value wrong")
            call assert(unwrap_str(get_index%value) == "String 3", "String list get index value wrong")
            call assert(.not. get_index%error, "String list get index error set")
            call assert(unwrap_str(get_key%value) == "String 6", "String list get key value wrong")
            call assert(.not. get_key%error, "String list get key error set")
            call assert(.not. associated(get_error), "String list get error was associated")
            print *,"String list test passed"
        end subroutine test_list_strings
end program test