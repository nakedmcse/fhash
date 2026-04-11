! Fortran generic array and hash table
module fhash
    use, intrinsic :: iso_fortran_env, only: int64
    implicit none

    type, public :: fhash_kv
        character(len=:), allocatable :: key
        class(*), allocatable :: value
        logical :: error = .false.
    end type fhash_kv

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
    end type fhash_ht

    contains

        ! HT related
        function fnv1a_hash(key) result (res)
            integer(int64) :: res = 14695981039346656037
            integer(int64) :: fnv_prime = 1099511628211
            integer(int64) :: key_val
            integer :: i
            character(len=*) :: key

            do i = 1, len(key)
                key_val = iachar(key(i:i), kind=int64)
                res = ieor(res, key_val)
                res = res * fnv_prime
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

end module fhash