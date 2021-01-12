// c library test
// compile with the following flags
// cc -Wall -shared -fPIC -o libfunc.so function.c

#include <stdio.h>
int test(int num) 
{ 
	
    printf("hello from c function: test\n");
    printf("a = %i \n",num);
    return 0;

} 
