"""
Given an array of positive or negative integers

I= [i1,..,in]

you have to produce a sorted array P of the form

[ [p, sum of all ij of I for which p is a prime factor (p positive) of ij] ...]

P will be sorted by increasing order of the prime numbers. The final result has to be given as a string in Java, C#, C, C++ and as an array of arrays in other languages.

Example:

I = [12, 15] # result = [[2, 12], [3, 27], [5, 15]]

[2, 3, 5] is the list of all prime factors of the elements of I, hence the result.

Notes:

    It can happen that a sum is 0 if some numbers are negative!

Example: I = [15, 30, -45] 5 divides 15, 30 and (-45) so 5 appears in the result, the sum of the numbers for which 5 is a factor is 0 so we have [5, 0] in the result amongst others.

    In Fortran - as in any other language - the returned string is not permitted to contain any redundant trailing whitespace: you can use dynamically allocated character strings.

"""

import numpy as np

def find_prime_factors(n):
    # function to find all prime factors of n
    # repetition admited
    primes = [] 
    # start with candidate factor 2
    d = 2 
    # loop through numbers increasing d until d=sqrt(n)
    while d*d <= n:
        # check if d is a factor, and how many times
        while n%d==0: 
            primes.append(d)
            n //= d
        # increase the candidate
        d += 1 
    # the remaining value of n can also be a prime
    if n>1:
        primes.append(n)
    return primes

def sum_for_list(lst):
    # get all primes 
    primes = []
    for ith in lst: 
        primes_ith = find_prime_factors(abs(ith))
        primes += primes_ith
    primes = np.unique(primes)
    # now create the sorted output list 
    nums = np.array(lst)
    out = []
    for pf in primes: 
        mask = nums%pf == 0
        out.append([pf,np.sum(nums[mask])])
    return out

a = [-29804, -4209, -28265, -72769, -31744]
b = sum_for_list(a)
print(b)