"""Decorators with parameters: add functools.wraps"""

import functools

# Decorator with arguments (3 levels!!!)
def decoratorWithParameters(arg1, arg2, arg3):
    def wrap(func):
        @functools.wraps(func)
        def wrapped_func(*args, **kwargs):
            # ---
            # Do stuff previous to func
            print("="*30)
            print("Before func")
            print("decorator arguments: ", arg1, arg2, arg3)
            # ---
            # Execute func
            result = func(*args, **kwargs)
            # ---
            # Do stuff after func
            print("="*30)
            # ---
            return result
        return wrapped_func
    return wrap

# I create a decorated test function

@decoratorWithParameters(1,2,"asd")
def test(name):
    print(f"Hello {name}.")

print(test)