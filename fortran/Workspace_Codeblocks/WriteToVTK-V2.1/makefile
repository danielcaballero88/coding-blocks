LIB_WriteToVTK.o: LIB_WriteToVTK.f90
	gfortran -O3 -c LIB_WriteToVTK.f90

ImplementationVTK.o: ImplementationVTK.f90
	gfortran -O3 -c ImplementationVTK.f90

WriteToVTK: LIB_WriteToVTK.o ImplementationVTK.o
	gfortran -o "WriteToVTK" LIB_WriteToVTK.o ImplementationVTK.o

# all targets
all: WriteToVTK

clean:
	rm ImplementationVTK.o LIB_WriteToVTK.o lib_writetovtk.mod WriteToVTK
