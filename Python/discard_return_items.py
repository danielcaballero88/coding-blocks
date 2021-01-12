def myfunc(a):
    return a, a, a, a


# En este caso b agarra todos los items en un tuple (no hace falta asterisco)
b = myfunc(2) 
print('b: ', b)


# En este caso c agarra los tres ultimos items en una lista
b, *c = myfunc(2) 
print('b: ', b)
print('c: ', c)


# En este caso d agarra los dos ultimos items en una lista
b, c, *d = myfunc(2) 
print('b: ', b)
print('c: ', c)
print('d: ', d)


# En este caso e agarra el ultimo item en forma de lista
b, c, d, *e = myfunc(2) 
print('b: ', b)
print('c: ', c)
print('d: ', d)
print('e: ', e)


# En este caso f agarra nada y es una lista vacia    
b, c, d, e, *f = myfunc(2) 
print('b: ', b)
print('c: ', c)
print('d: ', d)
print('e: ', e)
print('f: ', f)
