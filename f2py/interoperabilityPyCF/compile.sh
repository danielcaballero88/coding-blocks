#!bin/bash

# compile fortran library
gfortran -Wall -shared -fPIC -o libtest.so libtest.f90

# compile c library
cc -Wall -shared -fPIC -o libfunc.so function.c

# run python script as interactive to keep open after running
#python -i libTest.py 
