# returns a tuple
def myfunc(a):
    return a, a, a, a

c = myfunc(2)
print(c)


# returns a list
def myfunc2(a):
    b = [a, a]
    return b, a

(a, b), c = myfunc2(3)
print(a, b, c)