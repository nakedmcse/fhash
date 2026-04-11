all: fhash test

fhash: fhash.f90
	gfortran -ffree-line-length-0 -o fhash.o -c fhash.f90
	ar rcs fhash.a fhash.o
	rm -f *.o

test: fhash.a test.f90
	gfortran -ffree-line-length-0 -o test test.f90 fhash.a

clean:
	rm -f *.o
	rm -f *.a
	rm -f *.mod
	rm -f test