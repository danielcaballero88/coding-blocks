import unittest

from example_package.myModule import suma, resta, multiplica, divide

class Test_myModule(unittest.TestCase):
    def test_suma(self):
        x = 5
        y = 7
        result = suma(x,y)
        self.assertEqual(result, 12)

    def test_resta(self):
        x = 5
        y = 7
        result = resta(x,y)
        self.assertEqual(result, -2)
    
    def test_multiplica(self):
        x = 5
        y = 7
        result = multiplica(x,y)
        self.assertEqual(result, 35)
    
    def test_divide(self):
        x = 5
        y = 7
        result = divide(x,y)
        self.assertEqual(result, 5/7)
    