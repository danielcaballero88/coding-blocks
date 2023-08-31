"""Decorators with parameters: add functools.wraps"""

import functools


# Decorator with arguments (3 levels!!!)
def decorator_with_arguments(arg1, arg2, arg3):
    """Example decorator 05, decorator with arguments and @functools.wraps."""
    def decorator(func):
        @functools.wraps(func)
        def wrapped_func(*args, **kwargs):
            # ---
            # Do stuff previous to func
            print("=" * 30)
            print("Before func")
            print("decorator arguments: ", arg1, arg2, arg3)
            # ---
            # Execute func
            result = func(*args, **kwargs)
            # ---
            # Do stuff after func
            print("=" * 30)
            # ---
            return result

        return wrapped_func

    return decorator


# I create a decorated test function


@decorator_with_arguments(1, 2, "asd")
def decorated_function(name):
    """Test decorated function."""
    print(f"Hello {name}.")


print(decorated_function)
