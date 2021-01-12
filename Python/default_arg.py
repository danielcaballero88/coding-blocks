def my_func(arg1, arg2=None):
    if arg2 is None:
        arg2 = [1, 2, 3, 4]
    arg2.append(arg1)
    return arg2

def main():
    a = 1
    aux = [1,2]
    b = my_func(a, aux)
    print(b)
    
if __name__ == '__main__':
    main()