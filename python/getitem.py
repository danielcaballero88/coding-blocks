import numpy as np


class my_array(object):
    def __init__(self, a, b):
        self.a = a 
        self.b = np.array(b)
        
    def __getitem__(self, item):
        # print(item)
        return self.b[item]
    
    def sum_val(self, val, item=slice(None,None,None)):
        self.b[item] += val
    

def main():
    a = my_array(1, range(10)) 
    a.b += 10
    a.sum_val(10,2)
    print(a.a)
    print(a.b) 
    print(a[:])
    print(a[1])
    print(a[4:])
    print(a[0:8:2])
    print(a[[0,2,7]])

        
if __name__ == '__main__':
    main()
    
