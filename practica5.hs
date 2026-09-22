-- PRACTICA 5


-- EJERCICIO 1

--ejercicio 1.1

longitud:: [t] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- ejericio 1.2

ultimo:: [t] -> t 
ultimo [x] = x
ultimo (x:xs) = ultimo xs

-- ejercicio 1.3

principio:: [t] -> [t]
principio [x] = []
principio (x:xs) = [x] ++ principio xs

-- ejercicio 1.4

reverso:: [t] -> [t]
reverso [x] = [x]
reverso (x:xs) = reverso xs ++ [x]

-- EJERCICIO 2

--ejercicio 2.1

pertenece:: (Eq t) => t -> [t] -> Bool
pertenece _ [] = False
pertenece t (x:xs) | t == x = True
                   | otherwise = pertenece t xs

-- ejercicio 2.2

todosIguales:: (Eq t) => [t] -> Bool
todosIguales [x] = True
todosIguales (x:y:xs) | x == y = todosIguales (x:xs)
                      | otherwise = False

-- ejercicio 2.3

todosDistintos:: (Eq t) => [t] -> Bool
todosDistintos [x] = True
todosDistintos (x:xs) | pertenece x xs = False
                      | otherwise = todosDistintos xs

-- ejercicio 2.4

hayRepetidos:: (Eq t) => [t] -> Bool
hayRepetidos [x] = False
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise = hayRepetidos xs

-- ejercicio 2.5 

quitar:: (Eq t) => t -> [t] -> [t]
quitar e [] = []
quitar e (x:xs) | e == x = xs
                | otherwise = x:(quitar e xs)

-- ejercicio 2.6

quitarTodos:: (Eq t) => t -> [t] -> [t]
quitarTodos e [] = []
quitarTodos e (x:xs) | e == x = quitarTodos e xs
                     | otherwise = [x] ++ (quitarTodos e xs)

-- ejercicio 2.7

eliminarRepetidos:: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = [x] ++ eliminarRepetidos (quitarTodos x xs)

-- ejercicio 2.8

mismosElementos:: (Eq t) => [t] -> [t] -> Bool
mismosElementos [] [] = True
mismosElementos [] _ = False
mismosElementos _ [] = False
mismosElementos (x:xs) ys | pertenece x ys = mismosElementos (quitarTodos x xs) (quitarTodos x ys)
                          | otherwise = False

-- ejercicio 2.9

capicua:: (Eq t) => [t] -> Bool
capicua [] = True
capicua (x:xs) | (x:xs) == reverso (x:xs) = True
               | otherwise = False

-- EJERCICIO 3

-- ejercicio 3.1

sumatoria:: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

-- ejercicio 3.2

productoria:: [Integer] -> Integer 
productoria [] = 0
productoria (x:xs) = x * (productoria xs)

-- ejercicio 3.3 

maximo:: [Integer] -> Integer 
maximo [] = 0
maximo (x:xs) | x > maximo xs = x
                | otherwise = maximo xs

-- ejercicio 3.4

sumarN:: Integer -> [Integer] -> [Integer]
sumarN n [] = []
sumarN n (x:xs) = [x+n] ++ sumarN n xs

-- ejercicio 3.5

sumarElPrimero:: [Integer] -> [Integer]
sumarElPrimero (x:xs) = sumarN x (x:xs)

-- ejercicio 3.6

sumarElUltimo:: [Integer] -> [Integer]
sumarElUltimo xs = sumarN (ultimo xs) xs


-- ejercicio 3.7

pares:: [Integer] -> [Integer]
pares [] = []
pares (x:xs) | mod x 2 == 0 = [x] ++ pares xs
             | otherwise = pares xs

-- ejercicio 3.8

multiplosDeN:: Integer -> [Integer] -> [Integer]
multiplosDeN n [] = []
multiplosDeN n (x:xs) | mod x n == 0 = x: (multiplosDeN n xs)
                      | otherwise = multiplosDeN n xs
                      
-- ejercicio 3.9 

ordenar:: [Integer] -> [Integer]
ordenar [] = []
ordenar xs = ordenar(quitarTodos (maximo xs) xs) ++ [maximo xs]

-- EJERCICIO 4

-- 4a

sacarBlancosRepetidos:: [Char] -> [Char]
sacarBlancosRepetidos [] = []
sacarBlancosRepetidos [x] = [x]
sacarBlancosRepetidos (x:y:ys) | (x== ' ') && (y == ' ') = sacarBlancosRepetidos (y:ys)
                               | otherwise = x:(sacarBlancosRepetidos (y:ys))

-- ejercicio 4b

contarPalabras:: [Char] -> Integer
contarPalabras [] = 0
contarPalabras (x:xs) | (x == ' ') && (ultimo (x:xs) == ' ') = conteo (sacarBlancosRepetidos xs)
                      | (x == ' ') && (ultimo (x:xs) /= ' ') = conteo (sacarBlancosRepetidos (xs ++ " "))
                      | (x /= ' ') && (ultimo (x:xs) /= ' ') = conteo (sacarBlancosRepetidos ((x:xs) ++ " "))
                      | otherwise = conteo (sacarBlancosRepetidos (x:xs))

conteo:: [Char] -> Integer
conteo [] = 0
conteo (x:xs) | x == ' ' = 1 + conteo xs
              | otherwise = conteo xs

-- ejercicio 4c

palabras:: [Char] -> [[Char]]
palabras [] = []
palabras (x:xs) = palabrasLimpio (sacarBlancosRepetidos (x:xs))

primeraPalabra:: [Char] -> [Char]
primeraPalabra [] = []
primeraPalabra (x:xs) | x == ' ' = []
                      | otherwise = x:(primeraPalabra xs)

restoTexto:: [Char] -> [Char]
restoTexto [] = []
restoTexto (x:xs) | x == ' ' = xs
                  |otherwise = restoTexto xs

palabrasLimpio:: [Char] -> [[Char]]
palabrasLimpio [] = []
palabrasLimpio (x:xs) | x == ' ' = palabrasLimpio xs
                      | otherwise = (x: primeraPalabra xs) : (palabrasLimpio (restoTexto xs))

-- ejercicio 4d

palabraMasLarga:: [Char] -> [Char]
palabraMasLarga [] = []
palabraMasLarga (x:xs) = palabraMayor (palabras (x:xs))

palabraMayor:: [[Char]] -> [Char]
palabraMayor [] = []
palabraMayor [x] = x
palabraMayor (x:xs) | longitud x > longitud (palabraMayor xs) = x
                    | otherwise = palabraMayor xs

-- ejercicio 4e

aplanar:: [[Char]] -> [Char]
aplanar [] = []
aplanar [x] = x
aplanar (x:xs) = x ++ aplanar xs 

-- ejercicio 4f

aplanarConBlancos:: [[Char]] -> [Char]
aplanarConBlancos [] = []
aplanarConBlancos [x] = x
aplanarConBlancos (x:xs) = x ++ " " ++ aplanar xs

-- ejerciciio 4g 

aplanarConNBlancos:: [[Char]] -> Integer -> [Char]
aplanarConNBlancos [] n = []
aplanarConNBlancos [x] n = x
aplanarConNBlancos (x:xs) n = x ++ (multiplicarBlancos " " n) ++ aplanarConNBlancos xs n

multiplicarBlancos:: [Char] -> Integer -> [Char]
multiplicarBlancos [] n = []
multiplicarBlancos _ 0 = []
multiplicarBlancos xs n = xs ++ multiplicarBlancos xs (n-1)

-- EJERCICIO 5

-- ejercicio 5.1

sumaAcumulada:: (Num t) => [t] -> [t]
sumaAcumulada [] = []
sumaAcumulada [x] = [x]
sumaAcumulada (x:xs) = x : (sumarTodos x (sumaAcumulada xs))

sumarTodos:: (Num t) => t -> [t] -> [t]
sumarTodos _ [] = []
sumarTodos n (y:ys) = (n + y) : sumarTodos n ys


-- ejercicio 5.2
descomponerEnPrimos:: [Integer] -> [[Integer]]
descomponerEnPrimos [] = []
descomponerEnPrimos (x:xs) = susDivPrimos x : descomponerEnPrimos xs

susDivPrimos:: Integer -> [Integer]
susDivPrimos n = factorizar n 2

factorizar:: Integer -> Integer -> [Integer]
factorizar 1 _ = []
factorizar n d | mod n d == 0 = d : factorizar (div n d) d
               | otherwise = factorizar n (d + 1)


--filtrarPrimos:: [Integer] -> [Integer]
--filtrarPrimos [] = []
--filtrarPrimos (x:xs) | esPrimo x = x : filtrarPrimos xs
--                     | otherwise = filtrarPrimos xs

esPrimo:: Integer -> Bool
esPrimo n | longitud (divide n n) == 2 = True
          | otherwise = False


esDivisor:: Integer -> Integer -> Bool      -- primero me fijo que cada numero que pruebe sea divisor de n
esDivisor n i | mod n i == 0 = True
              | otherwise = False

divide:: Integer -> Integer -> [Integer]     -- busco los divispres de i hasta 0 que dividan a n y los guardo en una lista
divide _ 0 = []
divide n i | (esDivisor n i) = divide n (i-1) ++ [i]
           | otherwise = divide n (i-1)


-- EJERCICIO 6

-- ejercicio 6a

enLosContactos:: [Char] -> [([Char], [Char])] -> Bool
enLosContactos _ [] = False
enLosContactos nom ((nomb, tel):xs) | nom == nomb = True
                                    | otherwise = enLosContactos nom xs


-- ejercicio 6b

agregarContacto:: ([Char], [Char]) -> [([Char], [Char])] -> [([Char], [Char])]
agregarContacto [] [] = []
agregarContacto [] (x:xs) = x:xs
agregarContacto (nomb, tel) [] = [(nomb, tel)]
agregarContacto (nomb, tel) (x:xs) | (enLosContactos nomb (x:xs) == True) 


elNombre:: ([Char], [Char]) -> [Char]
elNombre [] = []
elNombre (nombre, tel) = nombre

elTelefono:: ([Char], [Char]) -> [Char]
elTelefono [] = []
elTelefono (nombre, tel) = tel

