"""Simple decorators: adding functools.wraps decorator
This will help the decorated function to keep information about itself
(its name and documentation for instance, otherwise
it will be linked to the decorator information)"""

import functools

# A decorator is a function that wraps another function
# Therefore it can change what the function does

# I create a decorator WITHOUT functools.wraps
def function_decorator(func):
    def wrapped_func(*args, **kwargs):
        # ---
        # Do something before the function is executed
        print('='*30)
        # ---
        # Execute function
        func(*args, **kwargs)
        # ---
        # Do something after the function has been executed
        print('='*30)
        # ---
    return wrapped_func

# I create a decorated test function
@function_decorator
def test(name):
    print(f"Hello {name}.")

print(test) # <function function_decorator.<locals>.wrapped_func at 0x7f195951ec10>

# I repeat but with a decorator with functools.wraps
def function_decorator(func):
    @functools.wraps(func)
    def wrapped_func(*args, **kwargs):
        # ---
        # Do something before the function is executed
        print('='*30)
        # ---
        # Execute function
        func(*args, **kwargs)
        # ---
        # Do something after the function has been executed
        print('='*30)
        # ---
    return wrapped_func

# I create a test function to be decorated
@function_decorator
def test(name):
    print(f"Hello {name}.")

print(test) # <function test at 0x7f195951edc0>