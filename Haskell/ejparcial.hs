
-- SISTEMA DE STOCK
-- ejercicio 1

generarStock:: [String] -> [(String, Integer)]
generarStock [] = []
generarStock (x:xs) = agregarStock x (generarStock xs)

agregarStock:: String -> [(String, Integer)] -> [(String, Integer)]
agregarStock e [] = [(e,1)]
agregarStock e ((prod, cant): xs) | e == prod = (prod, cant + 1):xs
                                  | otherwise = (prod, cant):(agregarStock e xs)

-- ejercicio 2

stockDeProducto:: [(String, Integer)] -> String -> Integer
stockDeProducto [] x = 0
stockDeProducto ((prod, e):xs) x | x == prod = e
                                    | otherwise = stockDeProducto xs x

-- ejercicio 3

dineroEnStock:: [(String, Integer)] -> [(String, Float)] -> Float
dineroEnStock [] [] = 0
dineroEnStock (x:xs) [] = 0
dineroEnStock [] (y:ys) = 0
dineroEnStock ((prod, cant):xs) ((prod1, precio):ys) | prod == prod1 = (fromIntegral cant * precio) + dineroEnStock xs ((prod1, precio):ys)
                                                     | otherwise = dineroEnStock [(prod, cant)] ys + dineroEnStock xs ((prod1, precio):ys)

dineroEnStock1 :: [(String, Integer)] -> [(String, Float)] -> Float
dineroEnStock1 _ [] = 0
dineroEnStock1 stock ((prod, precio):ys) = 
    (fromIntegral (stockDeProducto stock prod) * precio) + dineroEnStock1 stock ys

-- ejercicio 4

aplicarOferta:: [(String, Integer)] -> [(String, Float)] -> [(String, Float)]
aplicarOferta _ [] = []
aplicarOferta [] _ = []
aplicarOferta ((pr1, cant):xs) ((pr2, precio):ys) | (stockDeProducto ((pr1, cant):xs) pr1 > 10) && (pr1 == pr2) = [(pr2,off)] ++ aplicarOferta xs ys -- me fijo si el prod es el mismo en ambas listas y le aplico el descuento, luego les saco la cabeza y sigo con recursión
                                                  | stockDeProducto ((pr1, cant):xs) pr1 > 10 = [(pr2, precio)] ++ aplicarOferta ((pr1,cant): xs) ys -- si no son el mismo prod voy buscando el prod de x:xs en la lista de ys (y no es porque ya lo compare)
                                                  | (stockDeProducto ((pr1, cant):xs) pr1 <= 10) && (pr1 == pr2) = [(pr2, precio)] ++ aplicarOferta xs ys
                                                  | otherwise = aplicarOferta xs ((pr2, precio):ys)                                               -- si la cantidad no es
                                                  where off = precio * 0.80



-- SOPA DE NUMEROS

-- ejercicio 5 

maximo:: [[Integer]] -> Integer
maximo [] = 0
maximo [f] = mayorLista f
maximo (f:fs) = numMayor (mayorLista f) (maximo fs)

mayorLista:: [Integer] -> Integer
mayorLista [] = 0
mayorLista [x] = x
mayorLista (x:xs) | x > mayorLista xs = x
                  | otherwise = mayorLista xs

numMayor:: Integer -> Integer -> Integer
numMayor a b | a > b = a
             | otherwise = b


-- ejercicio 6

cantRepe:: Integer -> [Integer] -> Integer
cantRepe e [] = 0 
cantRepe e (x:xs) | e == x = 1 + cantRepe e xs
                  | otherwise = cantRepe e xs

unaLista:: [[Integer]] -> [Integer]
unaLista [] = []
unaLista (x:xs) = x ++ unaLista xs

masRepesEnLista:: [Integer] -> Integer
masRepesEnLista [y] = y
masRepesEnLista (y:ys) | cantRepe y (y:ys) >= cantRepe (masRepesEnLista ys) (y:ys) = y
                       | otherwise = masRepesEnLista ys

masRepetido:: [[Integer]] -> Integer
masRepetido (f:fs) = masRepesEnLista (unaLista (f:fs))


-- ejercicio 7

elemento:: [Integer] -> Integer -> Integer
elemento (x:xs) 1 = x
elemento (x:xs) n = elemento xs (n-1)

posicion:: [[Integer]] -> [Integer] -> Integer
posicion (f:fs) [1, col] = elemento f col
posicion (f:fs) [fila, col] = posicion fs [fila - 1, col]

valoresDeCamino:: [[Integer]] -> [[Integer]] -> [Integer]
valoresDeCamino t [] = []
valoresDeCamino t (y:ys) = (posicion t y):(valoresDeCamino t ys)


-- ejercico 8

fibonacci:: Integer ->Integer
fibonacci 0 = 0 
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1 ) + fibonacci (x - 2)

esFibonacci :: Integer -> Bool
esFibonacci x = calculoAux x 0
    
calculoAux  :: Integer -> Integer -> Bool
calculoAux x n | x == fibonacci (n+1) = True
               | x < fibonacci (n+1) = False
               | otherwise = calculoAux x (n+1)


esCaminoFibo:: [Integer] -> Integer -> Bool
esCaminoFibo [] _ = True
esCaminoFibo (x:xs) i = (x == fibonacci i) &&  (esCaminoFibo xs (i+1))
                            


-- PERFECTOS AMIGOS

-- ejercico 9

divisoresPropios:: Integer -> [Integer]
divisoresPropios 1 = []
divisoresPropios n = divide n (n-1)

divide:: Integer -> Integer -> [Integer]
divide _ 0 = []
divide n i | (esDivisor n i) = (divide n (i-1)) ++ [i]
           | otherwise = divide n (i-1)

esDivisor:: Integer -> Integer -> Bool
esDivisor n i = mod n i == 0 


-- ejercico 10

sonAmigos:: Integer -> Integer -> Bool
sonAmigos n m | (sumaDivisores (divisoresPropios n)) == m &&  (sumaDivisores (divisoresPropios m)) == n = True
              | otherwise = False


sumaDivisores:: [Integer] -> Integer
sumaDivisores [] = 0
sumaDivisores [x] = x
sumaDivisores (x:xs) = x + (sumaDivisores xs)


-- ejercicio 11


numPerfecto:: Integer -> Bool
numPerfecto t | sumaDivisores (divisoresPropios t) == t = True
              | otherwise = False

encontrarPerfecto:: Integer -> Integer
encontrarPerfecto n = auxiliarPerf 1 n

auxiliarPerf:: Integer -> Integer -> Integer
auxiliarPerf c 1 | numPerfecto c = c
                 | otherwise = auxiliarPerf (c + 1) 1
auxiliarPerf c n | numPerfecto c = auxiliarPerf (c+1) (n-1)
                 | otherwise = auxiliarPerf (c+1) n

losPrimerosNPerfectos:: Integer -> [Integer]
losPrimerosNPerfectos 0 = []
losPrimerosNPerfectos n = losPrimerosNPerfectos (n-1) ++ [encontrarPerfecto n]


-- ejercico 12

listaDeAmigos:: [Integer] -> [(Integer, Integer)]
listaDeAmigos [] = []
listaDeAmigos (f:fs) = (encontrarAmigos f fs) ++ (listaDeAmigos fs)


encontrarAmigos:: Integer -> [Integer] -> [(Integer,Integer)]
encontrarAmigos _ [] = []
encontrarAmigos x (y:ys) | (sonAmigos x y) == True = [(x,y)]
                           | otherwise = encontrarAmigos y ys
