# FHASH
![tests](https://github.com/nakedmcse/fhash/actions/workflows/build.yml/badge.svg)
[![GitHub issues](https://img.shields.io/github/issues/nakedmcse/fhash.png)](https://github.com/nakedmcse/fhash/issues)
[![last-commit](https://img.shields.io/github/last-commit/nakedmcse/fhash)](https://github.com/nakedmcse/fhash/commits/master)

A lightweight, simple to use, pure FORTRAN implementation of a generic hash table and dynamic array.

## Building

Clone the repository and then build the module:
```shell
git clone https://github.com/nakedmcse/fhash.git
cd fhash
make fhash
```

Then copy the `fhash.a` file to your projects directory.

You can then add the following to the top of your program:
```fortran
use fhash
```

Then to compile, use the following:
```shell
gfortran -o your_program your_program.f90 fhash.a
```

## Testing

The repository comes with a set of unit tests in `test.f90`, that can be built and run using the following:
```shell
make all
./test
```

## Generic Type Implementation

Generic types are used for the items in both the array and hashtable (the hashtable uses a dynamic array as its backing store).

These are implemented using the `class(*)` construction.  This allows you to use simple `item = value` syntax when adding 
items to either the array or hashtable.  It is truly generic supporting any type - including derived objects.

Reading the items back, however requires you to write a simple unwrap function as FORTRAN requires
you to specify the type when reading the value.  Conceptually this is similar to casting a void pointer in C.

The unwrap function itself is very simple, but will need tailored to the type you are using:

```fortran
! Example INTEGER unwrap
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

! Example STRING unwrap
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
```

## Hash Table Usage

Conceptually this is just initializing the hashtable to a given size, and then using set and get.
The sizing of the hash table will determine how it grows and how often the entries will be rehashed -
it will double in size when the size limit is hit and all entries will be rehashed.

```fortran
program hash_table_example
    use fhash
    implicit none
    type(fhash_ht) :: hashtable
    type(fhash_kv) :: item
    
    call hashtable%init(256)
    
    item%key = "one"
    item%value = "item one"
    call hashtable%set(item)

    item%key = "two"
    item%value = "item two"
    call hashtable%set(item)

    item%key = "three"
    item%value = "item three"
    call hashtable%set(item)

    item%key = "two"
    item%value = "updated item two"
    call hashtable%set(item)
    
    call hashtable%get("two", item)
    print *,unwrap_str(item%value)
    
    call hashtable%remove("three")
    call hashtable%get("three", item)
    print *, item%error  ! this will be true as three was removed
    
    contains

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
end program hash_table_example
```

## Dynamic Array Usage

Conceptually this is simply creating a variable of the type `fhash_array` and then using append to add items, or pop to remove the last item added.
It can be accessed like an ordinary array also.

```fortran
program array_example
    use fhash
    implicit none
    type(fhash_array) :: array
    type(fhash_kv) :: item, popped
    
    item%value = "one"
    call array%append(item)
    
    item%value = "two"
    call array%append(item)
    
    item%value = "three"
    call array%append(item)
    
    call array%pop(popped)
    print *,unwrap_str(popped%value)
    
    print *,unwrap_str(array%items(1)%value)
    print *,array%count

    contains

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
end program array_example
```