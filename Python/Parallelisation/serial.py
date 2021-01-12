# importing
import time


def fibonacci_sequence_of(num):    
    first_number = 0    
    second_number = 1    
    # Need to convert raw user input into integer for computation-
    num = int(num)    
    if num == 0:    
        print ("Fibonacci of {} is {}".format(num,num))    
    elif num == 1:    
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
    print(input_values)    
    for i in input_values:    
        fibonacci_sequence_of(i)    
        tic = time.time()    
    time_taken=round((tic-toc)*1000, 1)    
    print ("It takes {} milli-seconds to calculate the fibonacci of {} concurrently".format(time_taken,input_number))
