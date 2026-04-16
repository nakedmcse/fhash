! Fortran generic array and hash table
module fhash
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    type, public :: fhash_kv
        character(len=:), allocatable :: key
        class(*), allocatable :: value
        logical :: error = .false.
    end type fhash_kv

    type, public, extends(fhash_kv) :: fhash_list_node
        type(fhash_list_node), pointer :: next => null()
        type(fhash_list_node), pointer :: previous => null()
    end type fhash_list_node

    type, public :: fhash_array
        type(fhash_kv), dimension(:), allocatable :: items
        integer :: count = 0
        contains
            procedure append
            procedure pop
    end type fhash_array

    type, public :: fhash_ht
        type(fhash_array) :: storage
        contains
            procedure init
            procedure rehash
            procedure set
            procedure get
            procedure remove
    end type fhash_ht

    type, public :: fhash_list
        type(fhash_list_node), pointer :: header => null()
        type(fhash_list_node), pointer :: footer => null()
        integer :: count = 0
        contains
            procedure append_node
            procedure prepend_node
            procedure pop_node
            procedure shift_node
            procedure get_node
            procedure set_node
            procedure remove_node
    end type fhash_list

    contains

        ! HT related
        function fnv1a_hash(key) result (res)
            integer(int64) :: res
            integer(int64) :: fnv_prime = 1099511628211_int64
            integer(int64) :: key_val
            integer :: i
            character(len=*) :: key

            res = -3750763034362895579_int64
            do i = 1, len(key)
                key_val = iachar(key(i:i), kind=int64)
                res = ieor(res, key_val) * fnv_prime
            end do
        end function fnv1a_hash

        subroutine init(this, size)
            class(fhash_ht) :: this
            integer :: size

            if(.not. allocated(this%storage%items)) then
                allocate(this%storage%items(size))
            end if
        end subroutine init

        subroutine rehash(this, new_size)
            class(fhash_ht) :: this
            integer, intent(in) :: new_size
            integer :: i, offset, idx, mod_idx, old_size
            type(fhash_kv), dimension(:), allocatable :: temp

            if (.not. allocated(this%storage%items)) return
            old_size = size(this%storage%items)
            if (new_size <= old_size) return

            allocate(temp(new_size))
            do i = 1, old_size
                if (.not. allocated(this%storage%items(i)%key)) cycle

                idx = modulo(fnv1a_hash(this%storage%items(i)%key), new_size) + 1
                do offset = 0, new_size - 1
                    mod_idx = modulo(idx - 1 + offset, new_size) + 1
                    if (.not. allocated(temp(mod_idx)%key)) then
                        temp(mod_idx) = this%storage%items(i)
                        exit
                    end if
                end do
            end do
            call move_alloc(temp, this%storage%items)
        end subroutine rehash

        subroutine get(this, key, res)
            class(fhash_ht) :: this
            character(len=*), intent(in) :: key
            type(fhash_kv), intent(out) :: res
            integer :: idx, mod_idx, offset, capacity

            res%error = .true.
            if (.not. allocated(this%storage%items)) return
            capacity = size(this%storage%items)
            if (capacity == 0) return

            idx = modulo(fnv1a_hash(key), capacity) + 1

            do offset = 0, capacity - 1
                mod_idx = modulo(idx - 1 + offset, capacity) + 1
                if (.not. allocated(this%storage%items(mod_idx)%key)) then
                    return
                elseif (this%storage%items(mod_idx)%key == key) then
                    res = this%storage%items(mod_idx)
                    res%error = .false.
                    return
                end if
            end do
        end subroutine get

        subroutine set(this, value)
            class(fhash_ht) :: this
            type(fhash_kv), intent(in) :: value
            integer :: idx, mod_idx, offset, capacity

            if (.not. allocated(this%storage%items)) then
                call this%init(256)
            end if
            capacity = size(this%storage%items)
            if (this%storage%count == capacity) then
                call this%rehash(capacity * 2)
                capacity = size(this%storage%items)
            end if

            idx = modulo(fnv1a_hash(value%key), capacity) + 1
            do offset = 0, capacity - 1
                mod_idx = modulo(idx - 1 + offset, capacity) + 1
                if (.not. allocated(this%storage%items(mod_idx)%key)) then
                    this%storage%items(mod_idx)%key = value%key
                    this%storage%items(mod_idx)%value = value%value
                    this%storage%count = this%storage%count + 1
                    return
                elseif (this%storage%items(mod_idx)%key == value%key) then
                    this%storage%items(mod_idx)%value = value%value
                    return
                end if
            end do
        end subroutine set

        subroutine remove(this, key)
            class(fhash_ht) :: this
            character(len=*) :: key
            integer :: idx, mod_idx, offset, capacity

            if (.not. allocated(this%storage%items)) return
            capacity = size(this%storage%items)
            if (capacity == 0) return

            idx = modulo(fnv1a_hash(key), capacity) + 1

            do offset = 0, capacity - 1
                mod_idx = modulo(idx - 1 + offset, capacity) + 1
                if (.not. allocated(this%storage%items(mod_idx)%key)) then
                    return
                elseif (this%storage%items(mod_idx)%key == key) then
                    deallocate(this%storage%items(mod_idx)%key)
                    deallocate(this%storage%items(mod_idx)%value)
                    return
                end if
            end do
        end subroutine remove

        ! Array related
        subroutine append(this, value)
            class(fhash_array) :: this
            type(fhash_kv) :: value
            type(fhash_kv), dimension(:), allocatable :: temp

            if (.not. allocated(this%items)) then
                allocate(this%items(256))
            elseif (size(this%items) == this%count) then
                allocate(temp(this%count * 2))
                temp(1:this%count) = this%items(1:this%count)
                call move_alloc(temp,this%items)
            end if

            this%count = this%count + 1
            this%items(this%count) = value
        end subroutine append

        subroutine pop(this, res)
            class(fhash_array) :: this
            type(fhash_kv) :: res

            if (this%count == 0) then
                res%error = .true.
            else
                res = this%items(this%count)
                this%count = this%count - 1
            end if
        end subroutine pop

        ! List related
        subroutine append_node(this, node)
            class(fhash_list) :: this
            class(fhash_list_node) :: node
            class(fhash_list_node), pointer :: new_node

            allocate(new_node)
            new_node%key = node%key
            if (allocated(node%value)) new_node%value = node%value
            new_node%error = node%error
            nullify(new_node%next)
            nullify(new_node%previous)
            this%count = this%count + 1

            if(.not. associated(this%footer)) then
                this%header => new_node
                this%footer => new_node
                return
            end if

            new_node%previous => this%footer
            this%footer%next => new_node
            this%footer => new_node
        end subroutine append_node

        subroutine prepend_node(this, node)
            class(fhash_list) :: this
            class(fhash_list_node) :: node
            class(fhash_list_node), pointer :: new_node

            allocate(new_node)
            new_node%key = node%key
            if (allocated(node%value)) new_node%value = node%value
            new_node%error = node%error
            nullify(new_node%next)
            nullify(new_node%previous)
            this%count = this%count + 1

            if(.not. associated(this%header)) then
                this%header => new_node
                this%footer => new_node
                return
            end if

            new_node%next => this%header
            this%header%previous => new_node
            this%header => new_node
        end subroutine prepend_node

        subroutine pop_node(this, node)
            class(fhash_list) :: this
            class(fhash_list_node) :: node

            if(.not. associated(this%footer)) then
                node%error = .true.
                return
            end if

            node%key = this%footer%key
            node%value = this%footer%value
            node%error = .false.
            node%next => this%footer%next
            node%previous => this%footer%previous
            deallocate(this%footer)
            this%footer => node%previous
            this%count = this%count - 1
        end subroutine pop_node

        subroutine shift_node(this, node)
            class(fhash_list) :: this
            class(fhash_list_node) :: node

            if(.not. associated(this%header)) then
                node%error = .true.
                return
            end if

            node%key = this%header%key
            node%value = this%header%value
            node%error = .false.
            node%next => this%header%next
            node%previous => this%header%previous
            deallocate(this%header)
            this%header => node%next
            this%count = this%count - 1
        end subroutine shift_node

        subroutine get_node(this, node, key)
            class(fhash_list), intent(in) :: this
            type(fhash_list_node), intent(out) :: node
            class(*), intent(in) :: key

            type(fhash_list_node), pointer :: current
            integer :: idx

            node%error = .true.
            nullify(node%next)
            nullify(node%previous)
            if (allocated(node%key)) deallocate(node%key)
            if (allocated(node%value)) deallocate(node%value)

            current => this%header
            if (.not. associated(current)) return

            select type (key)
            type is (integer)
                idx = key
                if (idx < 1) return

                do while (associated(current))
                    if (idx == 1) then
                        node%key = current%key
                        if (allocated(current%value)) allocate(node%value, source=current%value)
                        node%error = .false.
                        node%next => current%next
                        node%previous => current%previous
                        return
                    end if

                    current => current%next
                    idx = idx - 1
                end do

            type is (character(*))
                do while (associated(current))
                    if (current%key == key) then
                        node%key = current%key
                        if (allocated(current%value)) allocate(node%value, source=current%value)
                        node%error = .false.
                        node%next => current%next
                        node%previous => current%previous
                        return
                    end if

                    current => current%next
                end do

            class default
                return
            end select
        end subroutine get_node

        subroutine set_node(this, node)
            class(fhash_list) :: this
            class(fhash_list_node) :: node
            ! TODO: Implement set node
        end subroutine set_node

        subroutine remove_node(this, key)
            class(fhash_list) :: this
            class(*) :: key ! integer index or string key
            ! TODO: Implement remove node
        end subroutine remove_node
end module fhash