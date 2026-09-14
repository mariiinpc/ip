--1a
longitud :: [t] -> Integer
longitud [] = 0 
longitud (x:xs) = 1 + longitud xs

--1b
ultimo :: [t] -> t
ultimo [x] = x
ultimo (x:xs) = ultimo xs

--1c
principio :: [t] -> [t] 
principio [x] = [] -- ponemos que cundo la lista tiene un solo elemento (osea que al hacer recursion queda el ultimo elemnto) sea vacia ya que no queremos agregarlo
principio (x:xs) = [x] ++ principio xs -- a medidia que separamos el primer numero le sumamos el segundo numero de la lista 
                                       -- ponemos [x] porque al hacer (x:xs), x pasa hacer un numero, y en principio xs no pnemos corchete ya que xs ya es un lista


--1d
reverso :: [t] -> [t]
reverso [] = []
reverso xs = [ultimo xs] ++ reverso (principio xs) -- aca usamo la funcion ultimo y comvertimos el numero que nos da en una lista 
                                                   -- del otro lado del ++ llamamo devuelta a la funcion pero le pasamos la lsita de principio que deveulve la lista sin el ultimo elemento
                                                   -- ya que el ultimo ya lo estamo usando, por lo que ahora el ultmo va a ser otro numero 

--2a
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e [] = False
pertenece e xs | e == head xs = True
               | otherwise = pertenece e (tail xs)

pertenece2 :: (Eq t) => t -> [t] -> Bool
pertenece2 e [] = False
pertenece2 e (x:xs) | e == x = True
                    | otherwise = pertenece2 e xs

--2b
todosIguales :: (Eq t) => [t] -> Bool
todosIguales [x] = True
todosIguales (x:y:xs)| x == y = todosIguales (y:xs) -- aca toma los dos primeros elementos de la lsita y los compara luego toma el segunodo y tercero y los vuelve a comprara asi sucesivamente
                     | otherwise = False

--2c
todosDistintos :: (Eq t) => [t] -> Bool
todosDistintos [x] = True
todosDistintos (x:xs) | pertenece x xs = False 
                      | otherwise = todosDistintos xs

--2d
hayRepetidos :: (Eq t) => [t] -> Bool
hayRepetidos [] = False
hayRepetidos [x] = False
hayRepetidos (x:xs) | pertenece x xs = True
                    | otherwise = hayRepetidos xs

--2e
quitar :: (Eq t) => t -> [t] -> [t]
quitar e [] = []
quitar e (x:xs) |e == x = xs -- como xs ya es una lista si yo lo ecribo como[xs] me va va dar una lista de lista [[xs]]
                | otherwise = x: (quitar e xs)

--2f
quitarTodos :: (Eq t ) => t -> [t] -> [t]
quitarTodos e [] = []
quitarTodos e (x:xs) | e == x = quitarTodos e xs
                     | otherwise = x: (quitarTodos e xs)

--2g
eliminarRepetidos :: (Eq t) => [t] -> [t]
eliminarRepetidos [] = []
eliminarRepetidos (x:xs) = x : eliminarRepetidos (quitarTodos x xs)

--3a
sumatoria :: [Integer] -> Integer
sumatoria [] = 0
sumatoria [x] = x
sumatoria (x:xs) = x + sumatoria xs

--3b
productoria :: [Integer] -> Integer
productoria [] = 1
productoria [x] = x
productoria (x:xs) = x * (productoria xs)

--3c
maximo :: [Integer] -> Integer
maximo [] = 0
maximo [x] = x
maximo (x:xs) | x > maximo xs = x
              | otherwise = maximo xs

--3d
sumarN :: Integer -> [Integer] -> [Integer]
sumarN n [] = []
sumarN n (x:xs) = [n+x] ++ sumarN n xs

--3e
sumarElPrimero :: [Integer] -> [Integer]
sumarElPrimero (x:xs) = [x+1] ++ sumarN x xs

--3f
sumarElUltimo :: [Integer] -> [Integer] 
sumarElUltimo xs = sumarN (ultimo xs) xs

--3g
pares :: [Integer] -> [Integer] 
pares [] = []
pares (x:xs) | mod x 2 == 0 = [x] ++ pares xs
             | otherwise = pares xs

--3h
multiplosDeN :: Integer -> [Integer] -> [Integer]
multiplosDeN n [x] = [x]
multiplosDeN n (x:xs) = [n*x] ++ multiplosDeN n xs

--3i
ordenar :: [Integer] -> [Integer]
ordenar [x] = [x]
ordenar (x:xs) | x < maximo xs = [x] ++ ordenar xs
               | otherwise = ordenar xs ++ [x]