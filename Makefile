main.elf: main.f90
	gfortran -cpp main.f90 -o main.elf

#!integer; type; assignment; constructor
f1.elf: f1.f90
	gfortran -cpp f1.f90 -o f1.elf
	
#!type, pointer;  allocate, source
f2.elf: f2.f90
	gfortran -cpp f2.f90 -o f2.elf
	
#!type, extends; class, pointer; select type
f3.elf: f3.f90
	gfortran -cpp f3.f90 -o f3.elf

#!procedure(abstract interface), pointer
f4.elf: f4.f90
	gfortran -cpp f4.f90 -o f4.elf -fmax-errors=1

#type bound procedure; binding name, polymorphism
f5.elf: f5.f90
	gfortran -cpp f5.f90 -o f5.elf

#type, abstract; procedure(abstract interface), deferred
f6.elf: f6.f90
	gfortran -cpp f6.f90 -o f6.elf

