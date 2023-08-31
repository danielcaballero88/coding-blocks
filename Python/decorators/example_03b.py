"""Simple decorators: It is important to get the result from the function! """

from functools import wraps

# A decorator is a function that wraps another function
# Therefore it can change what the function does


# I create a decorator WITHOUT functools.wraps
def function_decorator(func):
    """Example decorator 03b, get result from the decorated function."""
    @wraps(func)
    def wrapped_func(*args, **kwargs):
        # ---
        # Do something before the function is executed
        print("=" * 30)
        # ---
        # Execute function
        func_result = func(*args, **kwargs)  # IMPORTANT: get function result
        # ---
        # Do something after the function has been executed
        print("=" * 30)
        # ---
        # return the function result, or it can be modified by the decorator,
        # or return something else entirely, whatever is needed can be done.
        return func_result

    return wrapped_func


# I create a decorated test function
@function_decorator
def test(name):
    """Decorated test function."""
    print(f"Hello {name}.")
    return "Name printed"


RESULT = test("Dan")
print(RESULT)
