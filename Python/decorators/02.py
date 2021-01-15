"""Simple decorators: functions with arguments"""

# A decorator is a function that wraps another function
# Therefore it can change what the function does

# I create a decorator
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

# I create a test function to be decorated
def test(name):
    print(f"Hello {name}.")

# I call the function test, decorated
function_decorator(test)("Dani")

# Ah, but there is some cool syntax for this
@function_decorator
def test2(name):
    print(f"Hello again {name}")

test2("Dan")

# See that test2 is ALWAYS decorated,
# because the decorator is assigned to it at declaration, and not at execution.
# whereas with test, we decorated it at execution