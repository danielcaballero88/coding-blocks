"""Simple decorators: It is important to get the result from the function! """

from functools import wraps
# A decorator is a function that wraps another function
# Therefore it can change what the function does

# I create a decorator WITHOUT functools.wraps
def function_decorator(func):
    @wraps(func)
    def wrapped_func(*args, **kwargs):
        # ---
        # Do something before the function is executed
        print('='*30)
        # ---
        # Execute function
        result = func(*args, **kwargs) # IMPORTANT: get function result
        # ---
        # Do something after the function has been executed
        print('='*30)
        # ---
        return result # return the function result
    return wrapped_func

# I create a decorated test function
@function_decorator
def test(name):
    print(f"Hello {name}.")
    return "Name printed"

result = test("Dan")
print(result)
