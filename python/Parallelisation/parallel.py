#importing libraries-
import time
from multiprocessing import Pool

def fibonacci_sequence_of(num):
    first_number = 0
    second_number = 1
    # Need to convert raw user input into integer for computation-
    num = int(num)
    if num == 0:
        print ("Fibonacci of {} is {}".format(num,num))
    elif num ==1:
        print ("Fibonacci of {} is {}".format(num,num))
    else:
        for i in range(2,num): 
            new_number = first_number + second_number 
            first_number = second_number 
            second_number = new_number
        print ("Fibonacci of {} is {}".format(num,second_number))

if __name__ == '__main__':
    input_number = input("Provide comma-seperated-values for multiple values \nFabonacci of : ") 
    input_values=[]
    input_values = input_number.split(",")
    toc = time.time()
    # Making a pool object-
    pool = Pool()
    # Providing numerical values in parellel for computation using .map function
    # .map is a function that is gonna take a function and a list of something(numbes here) in interval and is going to map all those into the processors of our machine
    result = pool.map(fibonacci_sequence_of, input_values)
    tic = time.time()
  
    time_taken=round((tic-toc)*1000, 1)
    print ("It takes {} milli-seconds to calculate the fibonacci of {} in parellel ".format(time_taken,input_number))
    # Waiting for this process to finish running then close-
    pool.close()
    pool.join()
