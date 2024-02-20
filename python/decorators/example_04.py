"""Decorators with parameters"""


# Decorator with arguments (3 levels!!!)
def decorator_with_arguments(arg1, arg2, arg3):
    """Example decorator 04, decorator with arguments."""
    def decorator(func):
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
    """Decorated test function."""
    print(f"Hello {name}.")


decorated_function("Dan")


# Como seria esto sin usar la syntaxis copada?
def test_2(name):
    """Test function to be decorated"""
    print(f"Hello again {name}")


actual_decorator = decorator_with_arguments(6, 7, 8)
decorated_test_2 = actual_decorator(test_2)
decorated_test_2("Mr Dan")

# Neato!
# Debería hacer un dibujito o esquema de esto para publicarlo
