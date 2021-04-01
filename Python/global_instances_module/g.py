class Manager(object):
    def __init__(self):
        self.d = {}

    def get_var(self, name):
        if name not in self.d:
            self.d[name] = None
        return self.d[name]

    def set_var(self, name, val):
        self.d[name] = val

manager = Manager()

def get_gvar(name):
    return manager.get_var(name)

def set_gvar(name, val):
    manager.set_var(name, val)