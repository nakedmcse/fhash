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
        type(key_value), dimension(:), allocatable :: items
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
            integer :: new_size, i, j, idx
            type(fhash_kv), dimension(:), allocatable :: temp

            if (new_size <= size(this%storage%items)) return
            allocate(temp(new_size))

            do i = 1,size(this%storage%items)
                idx = modulo(fnv1a_hash(this%storage%items(i)%key), new_size)
                do j = idx, idx + new_size
                    if (.not. allocated(temp%items(modulo(j, new_size)))) then
                        temp%items(modulo(j, new_size)) = this%storage%items(i)
                        exit
                    end if
                end do
            end do

            call move_alloc(temp,this%storage%items)
        end subroutine

        subroutine get(this, key, res)
            class(fhash_ht) :: this
            character(len=*) :: key
            type(fhash_kv) :: res
            integer :: idx, mod_idx, i, capacity

            res%error = .true.
            capacity = size(this%storage%items)
            idx = modulo(fnv1a_hash(key), capacity)
            do i = idx, idx + capacity
                mod_idx = modulo(i,capacity)
                if (.not. allocated(this%storage%items(mod_idx)%key)) then
                    exit
                elseif (this%storage%items(mod_idx)%key == key) then
                    res = this%storage%items(mod_idx)
                    exit
                end if
            end do
        end subroutine

        subroutine set(this, value)
            class(fhash_ht) :: this
            type(fhash_kv) :: value, found
            integer :: idx, mod_idx, i, capacity

            capacity = size(this%storage%items)
            if (this%storage%count == capacity) then
                call this%rehash(capacity * 2)
            end if
            idx = modulo(fnv1a_hash(key), capacity)
            do i = idx, idx + capacity
                mod_idx = modulo(i,capacity)
                if (.not. allocated(this%storage%items(mod_idx)%key)) then
                    this%storage%items(mod_idx)%key = value%key
                    this%storage%items(mod_idx)%value = value%value
                    this%storage%count = this%storage%count + 1
                    exit
                elseif (this%storage%items(mod_idx)%key == key) then
                    this%storage%items(mod_idx)%value = value%value
                    exit
                end if
            end do
        end subroutine

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