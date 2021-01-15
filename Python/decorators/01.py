"""Simple decorators: no arguments"""

# A decorator is a function that wraps another function
# Therefore it can change what the function does

# I create a decorator
def function_decorator(func):
    def wrapped_func():
        # ---
        # Do something before the function is executed
        print('='*30)
        # ---
        # Execute function
        func()
        # ---
        # Do something after the function has been executed
        print('='*30)
        # ---
    return wrapped_func

# I create a test function to be decorated
def test():
    print("Hello")

# I call the function test, decorated
function_decorator(test)()

# Ah, but there is some cool syntax for this
@function_decorator
def test2():
    print("Hello 2")

test2()

# See that test2 is ALWAYS decorated,
# because the decorator is assigned to it at declaration, and not at execution.
# whereas with test, we decorated it at execution