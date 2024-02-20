from globals import g

def print_and_increment(name):
    var = getattr(g, name)
    print(var)
    var += 1
    setattr(g, name, var)