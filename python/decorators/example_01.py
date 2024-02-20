"""Simple decorators: no arguments"""

# A decorator is a function that wraps another function
# Therefore it can change what the function does


# I create a decorator
def function_decorator(func):
    """Example decorator 01, no arguments."""
    def wrapped_func():
        # ---
        # Do something before the function is executed
        print("=" * 30)
        # ---
        # Execute function
        func()
        # ---
        # Do something after the function has been executed
        print("=" * 30)
        # ---

    return wrapped_func


def function_not_decorated():
    """Test function to be wrapped by decorator."""
    print("Hello")


# I call the function test, decorated
decorated_function = function_decorator(function_not_decorated)
# (variable) def decorated_test() -> None
decorated_function()


# Ah, but there is some cool syntax for this
@function_decorator
def decorated_function_2():
    """Decorated test function."""
    print("Hello 2")


decorated_function_2()

# See that test2 is ALWAYS decorated,
# because the decorator is assigned to it at declaration, and not at execution.
# whereas with test, we decorated it at execution
