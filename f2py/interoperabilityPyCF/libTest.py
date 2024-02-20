# Python script to test interoperability features 
# with C and Fortran funtions/subroutines compiled 
# as libraries. 
#
# references: 
# https://dektoud.github.io/blog/post/fortran_dlls/
# https://www.geeksforgeeks.org/how-to-call-a-c-function-in-python/
#

import os
import ctypes

path = os.getcwd()   # se no es lo optimo dani, pero no me acuerdo la otra notacion

# loading libraries
flib = ctypes.CDLL(path+'/libtest.so')
clib = ctypes.CDLL(path+'/libfunc.so')

a = 10
a = ctypes.pointer(ctypes.c_int(a))   # parece ser algun tipo de casteo  
# a[0] = 10

# calls to fortran subroutines
flib.test1(a)
flib.test2(a)
# flib.test1(10) produces segfault

# call to c function
clib.test(10)
clib.test(a[0])
