import time

# create long list
a = list(range(0,10000,2))
# print(a)

# number to find
target = 500

# several alternatives
print( next(v for v in a if v==target) )
print( next(i for i,v in enumerate(a) if v==target) )
print( [v for v in a if v==target][0] )
print( [i for i,v in enumerate(a) if v==target][0] )

# number of iterations to time
nk = 1000

# most efficient method, stops after finding the first element
start = time.time() 
for k in range(nk):
    b = next(i for i,v in enumerate(a) if v==target)
end = time.time() 
print("time: ", end-start)

# less efficient method, first finds all, then returns the first
start = time.time() 
for k in range(nk):
    b = [i for i,v in enumerate(a) if v==target][0]
end = time.time() 
print("time: ", end-start)
