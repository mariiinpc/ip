-- SIMULACRO HASKELL

-- ejercicio 1


divisoresPropios:: Integer -> [Integer]     --busco los divisores propios de n y para 1 es lista vacia
divisoresPropios 1 = []                     -- porque notiene divisores menores a él
divisoresPropios n = divide n (n-1)         -- n-1 es el i en la funcion divide, así que busco todos los divisores menores a n

esDivisor:: Integer -> Integer -> Bool      -- primero me fijo que cada numero que pruebe sea divisor de n
esDivisor n i | mod n i == 0 = True
              | otherwise = False

divide:: Integer -> Integer -> [Integer]     -- busco los divispres de i hasta 0 que dividan a n y los guardo en una lista
divide _ 0 = []
divide n i | (esDivisor n i) = divide n (i-1) ++ [i]
           | otherwise = divide n (i-1)

sumaDivisores:: [Integer] -> Integer          -- busco el resultado de la suma de divisores de n
sumaDivisores [] = 0
sumaDivisores (x:xs) = x + sumaDivisores xs

numAbundante:: Integer -> Bool
numAbundante n | (sumaDivisores (divisoresPropios n ) ) > n = True
               | otherwise = False

longitud:: [Integer] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs


cantAbundantes:: Integer -> Integer -> [Integer]
cantAbundantes _ 1 = []
cantAbundantes n m | (n <= m) && (numAbundante m == True) = (cantAbundantes n (m-1)) ++ [m]
                   | (n <= m) && (numAbundante m == False)  = cantAbundantes n (m-1)
                   | otherwise = cantAbundantes n 1

f1:: Integer -> Integer -> Integer
f1 _ 1 = 0
f1 n m = longitud (cantAbundantes n m)

-- ejercicio 2

noPuedeFinal:: [(String, Integer, Integer)] -> Bool
noPuedeFinal [(materia, anio, cuatri)] | (anio < 2021) && (cuatri <= 2) = True
                                       | (anio == 2021) && (cuatri == 1) = True
                                       | otherwise = False

f2:: [(String, Integer, Integer)] -> [String]
f2 [] = []
f2 ((m, a, c):xs) | (noPuedeFinal [(m, a, c)] == True ) = f2 xs ++ [m]
                  | otherwise = f2 xs


-- ejercicio 3

--lugar:: [Integer] -> Integer -> [Integer]
--lugar [] t = []
--lugar (x:xs) t | t == x = 0
--               | otherwise = [1 + lugar xs t]

--indice:: [Integer] -> Integer -> Integer
--indice [x] 0 = x
--indice (x:xs) m | m == (lugar (x:xs) x) = x
--                | otherwise = indice xs m

--listaTope:: [Integer] -> Integer -> [Integer]
--listaTope [] _ = []
--listaTope (x:xs) n | (indice (x:xs) n) 

--listaPositivos:: [Integer] -> Integer -> [Integer]
--listaPositivos [x] 0 | x>= 0 = [indice [x] 0]
--                     | otherwise = []
--listaPositivos (x:xs) i | x>= 0 = [x] ++ listaPositivos xs

--f3:: [Integer] -> Integer -> [Integer]
--f3 [x] 0 | x>= 0 = [indice [x] 0]
--         | otherwise = []
--f3 (x:xs) i | x>= 0 = 


