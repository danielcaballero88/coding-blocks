from .myModule import suma, resta, multiplica, divide

class TwoNumberOperations(object):
    def __init__(self, x, y):
        self.x = x 
        self.y = y

    def suma(self):
        result = suma(self.x, self.y)
        return result

    def resta(self):
        result = resta(self.x, self.y)
        return result

    def multiplica(self):
        result = multiplica(self.x, self.y)
        return result

    def divide(self):
        result = divide(self.x, self.y)
        return result

    def binomio(self, n):
        result = self.suma()**n
        return result