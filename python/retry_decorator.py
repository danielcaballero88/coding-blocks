import time

# decorator
def retry(times=3, interval=3):
    def wrapper(func):
        def wrapper(*arg, **kwarg):
            for i in range(times):
                try:
                    return func(*arg, **kwarg)
                except Exception as e:
                    if i==times-1:
                        raise
                    time.sleep(interval)
                    continue
        return wrapper
    return wrapper

# usage
@retry(3, 1)
def getIntUserInput():
    userInput = int(input("Enter integer: "))
    return userInput

print(getIntUserInput())

