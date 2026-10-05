def suma (a: int, b: int) -> int:
    return a + b

#import unit                -- tiene que ser un archivo aparte test.py
#from guia1 import suma

#class test_suma(unittest.TestCase):
#   def suma_positiva_test(self):
#       self.assertEqual (suma(2,3), 5)

#   def suma_negativo(self):
#       self.assertEqual(suma(-4,-7), -11)

#if _name_ == '_main_':
#   unittest.main(verbosity=2)

# self.assertTrue(expression)           -- las otras opciones de testeo
#self.assertFalse(expression)
#self.assertNotEqual(a,b)
#self.assertGreater(a,b) -> a tiene que ser mayor a b
#self.assertLess(a,b) -> a tiene que ser menor a b
#self.assertIn(a,b) -> se fija que a esté en b
#self.assertNotIn(a,b)
#self.assetAlmostEqual(a,b,places=p) -> a es igual a b con una preisión de p decimales


def es_triada_pitagorica(a: int,b: int,c: int) -> bool:
    res: bool = a**2 + b**2 == c**2
    return res


def es_multiplo_de (n: int, m: int) -> bool:
    res: bool = n%m == 0
    return res

def doble_si_es_par (n:int) -> int:
    if n%2== 0:
        res = n*2
    else:
        res = n
    return res

def fahrenheit_a_celsius(temp:float) -> float:
    return (((temp-32)*5)/9)
def fahrenheit_a_celsius(temp:float) -> float:
    return (((temp-32)*5)/9)
    
def es_primo(n:int) -> bool:
    for i in range (2,n,1):
        if n== 2:
            res: bool = True
        elif n%i == 0:
            res: bool = False
    return res



print (es_primo(2))



import unittest
from guia1 import es_multiplo_de

class test_es_multiplo_de(unittest.TestCase):
    def test_es_multiplo_de(self):
        self.assertTrue (es_multiplo_de(4,2))


if __name__ == '__main__':
   unittest.main(verbosity=2)
