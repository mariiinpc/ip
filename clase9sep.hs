--numero_r:: Integer -> Integer
--numero_r x | x == 0 = 
--           | x ==
--

longitud:: [t] -> Integer
longitud [] = 0
longitud xs = longitud (tail xs) + 1


longitud1:: [t] -> Integer
longitud1 [] = 0
longitud1 (x:xs) = 1 + longitud1 xs 

sumatoria:: [Integer] -> Integer
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

quitar:: (Eq t) => t -> [t] -> [t]
quitar e [] = []
quitar e (x:xs) | e == x = xs
                | otherwise = (x:(quitar e xs))


maximo:: [Integer] -> Integer
maximo [] = 0
maximo (x:xs) |x > (maximo xs) = x
              |otherwise = maximo xs

maximo1:: [Integer] -> Integer
maximo1 (x:y:xs) | x > y = maximo1 (x:xs)
                 | otherwise = maximo1 (y:xs)

ordenar:: [Integer] -> [Integer]
ordenar [] = []
ordenar xs maxXs: ordenar (quitar maxXs xs)
    where maxXs = maximo xs

ordenarAsc:: [Integer] -> [Integer]
ordenarAsc [] = []
ordenarAsc xs = ordenarAsc (quitar maxXs xs) ++ [maxXs]
    where maxXs = maximo xs