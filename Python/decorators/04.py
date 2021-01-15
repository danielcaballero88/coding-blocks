"""Decorators with parameters"""

# Decorator with arguments (3 levels!!!)
def decoratorWithParameters(arg1, arg2, arg3):
    def wrap(func):
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

test("Dan")

# Como seria esto sin usar la syntaxis copada?

def test2(name):
    print(f"Hello again {name}")

decoratorWithParameters(6,7,8)(test2)("Mr Dan")

# Neato!
# Debería hacer un dibujito o esquema de esto para publicarlo