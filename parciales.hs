-- Primer cuatri 2025

-- ejercicio 1

hayPrimosGemelos:: Integer -> Integer -> Bool
hayPrimosGemelos n m | longitud (cantPrimosGemelos n m) > 0 = True
                     | otherwise = False

hayPrimosGemelos1:: Integer -> Integer -> Bool
hayPrimosGemelos1 n m | n>= m = False
                      | (sonPGemelos n (n+2)) = True
                      | otherwise = hayPrimosGemelos1 (n+1) m


absoluto:: Integer -> Integer
absoluto n | n < 0 = (-1) * n
           | otherwise = n

cantPrimosGemelos:: Integer -> Integer -> [[Integer]]
cantPrimosGemelos n m | n >= m = []
                      | sonPGemelos n (n+2) = [n ,n+2] : cantPrimosGemelos (n + 1) m
                      | otherwise = cantPrimosGemelos (n+1) m

sonPGemelos:: Integer -> Integer -> Bool
sonPGemelos _ 0 = False
sonPGemelos 0 _ = False
sonPGemelos n m | (esPrimo n == True) && (esPrimo m == True) && (absoluto (m-n) == 2) = True
                | otherwise = False

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


-- ejercicio 2

materiasTurnoTarde:: [(String , String, Integer, Integer)] -> [String]
materiasTurnoTarde [] = []
materiasTurnoTarde (x:xs) = materiasSinRepe (eliminarRepetidos (x:xs))


horarioInicio:: (String, String, Integer, Integer) -> Bool
horarioInicio (m, d, inicio, fin) | inicio >= 14 = True
                                  | otherwise = False

horarioFin:: (String, String, Integer, Integer) -> Bool
horarioFin (m, d, inicio, fin) | fin <= 17 = True
                               | otherwise = False

materiasSinRepe:: [(String, String, Integer, Integer)] -> [String]
materiasSinRepe [] = []
materiasSinRepe ((m, d, inicio, fin):xs) | ( horarioInicio (m, d, inicio, fin) == True) || (horarioFin (m, d, inicio, fin) == True) = [m] ++ materiasTurnoTarde xs
                                         | otherwise = materiasTurnoTarde xs

quitarTodos:: (Eq t) => t -> [t] -> [t]
quitarTodos e [] = []
quitarTodos e (x:xs) | e == x = quitarTodos e xs
                     | otherwise = [x] ++ (quitarTodos e xs)

eliminarRepetidos:: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = [x] ++ eliminarRepetidos (quitarTodos x xs)


-- ejercicio 3 

maximaSumaDeTresConsecutivos:: [Integer] -> Integer
maximaSumaDeTresConsecutivos [] = 0
maximaSumaDeTresConsecutivos [x,y,z] = x + y + z
maximaSumaDeTresConsecutivos (x:y:z:xs) | (x+y+z) > maximaSumaDeTresConsecutivos (y:z:xs) = x+y+z
                                        | otherwise = maximaSumaDeTresConsecutivos (y:z:xs)

-- ejercicio 4

sumaIesimaColumna:: [[Integer]] -> Integer -> Integer
sumaIesimaColumna [] _ = 0
sumaIesimaColumna (x:xs) n = (indice x n) + sumaIesimaColumna xs n

indice:: [Integer] -> Integer -> Integer
indice (x:_) 1 = x
indice (x:xs) m | 1 == m = x


-- TEMA 2

divisoresPropios:: Integer -> [Integer]
divisoresPropios 1 = []
divisoresPropios n = divide n (n-1)


sumatoria:: [Integer] -> Integer
sumatoria [] = 0
sumatoria [x] = 1
sumatoria (x:xs) = x + sumatoria xs

esNumAbudante:: Integer -> Bool
esNumAbudante n | sumatoria (divisoresPropios n) > n = True
                | otherwise = False

listaAbundantes:: Integer -> Integer -> [Integer]
listaAbundantes _ 1 = []
listaAbundantes n m | (n<= m) && (esNumAbudante m == True) = listaAbundantes n (m-1) ++ [m]
                    | (n<= m) && (esNumAbudante m == False) = listaAbundantes n (m-1)
                    | otherwise = []

cantidadNumerosAbundantes:: Integer -> Integer -> Integer
cantidadNumerosAbundantes n m = longitud (listaAbundantes n m)


-- Segundo Cuatrimestre 2024

-- ejercicio 1

mediaMovilN:: [Integer] -> Integer -> Float
mediaMovilN [] _ = 0
mediaMovilN xs n = promedio (listaUltimosN xs n)

promedio:: [Integer] -> Float
promedio xs = fromInteger (sumatoria xs) / fromInteger (longitud xs)

longitud:: [t] -> Integer
longitud [] = 0
longitud [x] = 1
longitud (x:xs) = 1 + longitud xs

ultimo:: [Integer] -> Integer
ultimo [x] = x
ultimo (x:xs) = ultimo xs

principio:: [Integer] -> [Integer]
principio [] = []
principio [x] = []
principio (x:xs) = [x] ++ principio xs

listaUltimosN:: [Integer] -> Integer -> [Integer]
listaUltimosN [] _ = []
listaUltimosN _ 0 = []
listaUltimosN (x:xs) n = (listaUltimosN (principio xs) (n-1)) ++ [ultimo (x:xs)]


-- Ejercicio 2

descomponerEnPrimos:: [Integer] -> [[Integer]]
descomponerEnPrimos [] = []
descomponerEnPrimos (x:xs) = susDivPrimos x : descomponerEnPrimos xs

esAtractivo:: Integer -> Bool
esAtractivo n = esPrimo(longitud (susDivPrimos n))

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



-- ejercico 3

palabraOrdenada:: [Char] -> Bool
palabraOrdenada palabra = estaOrdenada (fraseSinBlancos palabra)

fraseSinBlancos:: [Char] -> [Char]
fraseSinBlancos [] = []
fraseSinBlancos (x:xs) | x == ' ' = fraseSinBlancos xs
                       | otherwise = x:fraseSinBlancos xs

estaOrdenada:: [Char] -> Bool
estaOrdenada [] = True
estaOrdenada [_] = True
estaOrdenada (x:y:ys) | x <= y = estaOrdenada (y:ys)
                      | otherwise = False


-- ejercicio 4


-- Cuenta las apariciones de un carácter en una secuencia
contar :: Char -> [Char] -> Int
contar _ [] = 0
contar c (x:xs) | c == x    = 1 + contar c xs
                | otherwise = contar c xs

-- Calcula la longitud de una lista
longitud1 :: [Char] -> Int
longitud1 [] = 0
longitud1 (_:xs) = 1 + longitud1 xs

-- Filtra los espacios en blanco
quitarBlancos :: [Char] -> [Char]
quitarBlancos [] = []
quitarBlancos (x:xs) | x == ' '  = quitarBlancos xs
                     | otherwise = x : quitarBlancos xs

-- Verifica si todos los caracteres de la primera lista aparecen la misma cantidad en la segunda
mismasApariciones :: [Char] -> [Char] -> Bool
mismasApariciones [] _ = True
mismasApariciones (x:xs) s2 = contar x (x:xs) == contar x s2 && mismasApariciones xs s2

-- Función principal simplificada
similAnagrama :: [Char] -> [Char] -> Bool
similAnagrama p1 p2 = p1 /= p2 && longitud1 (quitarBlancos p1) == longitud1 (quitarBlancos p2) && mismasApariciones (quitarBlancos p1) (quitarBlancos p2)




