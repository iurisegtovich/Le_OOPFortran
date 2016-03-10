main.elf: main.f90
	gfortran -cpp main.f90 -o main.elf

f1.elf: f1.f90
	gfortran -cpp f1.f90 -o f1.elf
	
f2.elf: f2.f90
	gfortran -cpp f2.f90 -o f2.elf
	
f3.elf: f3.f90
	gfortran -cpp f3.f90 -o f3.elf
	
f4.elf: f4.f90
	gfortran -cpp f4.f90 -o f4.elf
