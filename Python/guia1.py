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



# clase 14 de octubre, ejercicios guia 1


#1.9

def palabras(s:list[str]) -> bool:
    for p in s:
        if len(p) > 7:
            return True
    return False

# 1.10
def reverso(s:str) -> str:
    res: str = ""
    for l in s:
        res = l + res
    return res

def es_palidromo(s:str) -> bool:
    if s == reverso(s):
        return True
    else:
        return False



#1.11

def iguales_consecutivos(s:list[int]) -> bool:
    for i in range(len(s)-2):
        if s[i] == s[i+1] & s[i] == s[i+2]:
            return True
        else:
            return False


#1.12

def minuscula(c:chr)-> chr:
    min = None
    if c == 'a' or c =='A':
        min = 'a'
    if c == 'e' or c =='E':
            min = 'e'
    if c == 'i' or c =='I':
            min = 'i'
    if c == 'o' or c =='O':
            min = 'o'
    if c == 'u' or c =='U':
            min = 'u'
    return min

def vocales_distintas(s:str) -> bool:
    vocales= []
    for l in s:
        if l in "aeiouAEIOU" and l not in vocales:
            if minuscula(l) not in vocales:
                vocales.append(l)
    return len(vocales)>= 3
        


def sacar_repetidos(s:str) -> list[str]:
    nuevapalabra= []
    for l in s:
        if l not in nuevapalabra:
            nuevapalabra.append(l)
    return nuevapalabra

#1.13

#def pos_secuencia_ordenada_mas_larga(s:list[int])-> int:
#    lista_ordenada=[]
#    while len(s) >= 1:
#     for i in range(len(s)):
#          if s[i]+1 <= s[i+1]:
#            lista_ordenada = lista_ordenada + [s[i]] + [s[i+1]]
#          else:
#               lista_ordenada.remove(s[i])
#
#          for n in lista_ordenada:
#               if s[i] == lista_ordenada[0]:
#                    s.remove(s[i-1])
#               for i in range(len(lista_ordenada)):
#                s.remove(lista_ordenada[i])
#    return 

def pos_secuencia_ordenada_mas_larga(s:list[int])-> int:
    mejor_inicio = 0
    mejor_longitud = 1
    inicio_actual= 0
    longitud_actual= 1
    for i in range (1,len(s)):
        if s[i-1] <= s[i]:
            longitud_actual += 1
        else:
             inicio_actual = 1
             longitud_actual = 1
        if longitud_actual > mejor_longitud:
             mejor_inicio = inicio_actual
             mejor_longitud = longitud_actual
    return mejor_inicio


#3.5.1

def pertenece_a_cada_uno_version_1(s:list[list[int]], n: int) -> list[bool]: #-> list[bool]:
    pass

def columna(m: list[list[int]], c: int) -> list[int]:
    col= []
    for i in range (len(m)):
        col.append(m[i][c])
    return col

def transponer(c: list[int]) -> list[list[int]]:
     traspuesta= []
     for i in range(len(c[0])):
          traspuesta.append(columna(c,i))
     return traspuesta
