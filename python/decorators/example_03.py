"""Simple decorators: adding functools.wraps decorator
This will help the decorated function to keep information about itself
(its name and documentation for instance, otherwise
it will be linked to the decorator information)"""

import functools

# A decorator is a function that wraps another function
# Therefore it can change what the function does


# I create a decorator WITHOUT functools.wraps
def function_decorator_1(func):
    """Example decorator 03, control case without @functool.wraps."""
    def wrapped_func(*args, **kwargs):
        # ---
        # Do something before the function is executed
        print("=" * 30)
        # ---
        # Execute function
        func(*args, **kwargs)
        # ---
        # Do something after the function has been executed
        print("=" * 30)
        # ---

    return wrapped_func


# I create a decorated test function
@function_decorator_1
def decorated_function(name):
    """Decorated test function."""
    print(f"Hello {name}.")


print(decorated_function)
# I do not get the actual function, but the decorator:
# <function function_decorator.<locals>.wrapped_func at 0x7f195951ec10>


# I repeat but with a decorator with functools.wraps
def function_decorator_2(func):
    """Example decorator 03, same as 02 but using @functools.wraps."""
    @functools.wraps(func)
    def wrapped_func(*args, **kwargs):
        # ---
        # Do something before the function is executed
        print("=" * 30)
        # ---
        # Execute function
        func(*args, **kwargs)
        # ---
        # Do something after the function has been executed
        print("=" * 30)
        # ---

    return wrapped_func


# I create a test function to be decorated
@function_decorator_2
def decorated_function_2(name):
    """Decorated test function."""
    print(f"Hello {name}.")


print(decorated_function_2)
# I get the actual decorated function:
# <function test at 0x7f195951edc0>
