"""Simple decorators: functions with arguments"""

# A decorator is a function that wraps another function
# Therefore it can change what the function does


# I create a decorator
def function_decorator(func):
    """Example decorator 02, function with arguments."""
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
def function_not_decorated(name):
    """Test function to be wrapped by decorator."""
    print(f"Hello {name}.")


# I call the function test, decorated
decorated_function = function_decorator(function_not_decorated)
# (variable) def decorated_test(*args: Any, **kwargs: Any) -> None
decorated_function("Dani")


# Ah, but there is some cool syntax for this
@function_decorator
def decorated_function_2(name):
    """Decorated test function."""
    print(f"Hello again {name}")


decorated_function_2("Dan")

# See that test2 is ALWAYS decorated,
# because the decorator is assigned to it at declaration, and not at execution.
# whereas with test, we decorated it at execution
