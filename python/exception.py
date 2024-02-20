def divide(a,b):
    if b==0:
        raise Exception(-1)
    else:
        return a/b
    
try:
    print(divide(1,0))
except Exception as err:
    print(err.args)
    print('err: ', err)