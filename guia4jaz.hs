import Distribution.Simple.Utils (xargs)
-- 1
fibonacci:: Integer ->Integer
fibonacci 0 = 0 
fibonacci 1 = 1
fibonacci x = fibonacci (x - 1 ) + fibonacci (x - 2)

-- 2
parteEntera :: Float -> Integer
parteEntera x | x < 1 = 0 
              | otherwise = 1 + parteEntera (x - 1)

--3
esDivisible :: Integer -> Integer ->Bool
esDivisible x y | x == 0 = True
                | x < y = False
                | otherwise = esDivisible (x - y) y -- si x es divisible por y , se le va restando ese y hasta llegar a 0, tira True, 
                                                    -- pero si se le resta y llegaa a ser menor a y , pero no 0 no es divisible y tira False

--4
sumaIpares :: Integer -> Integer 
sumaIpares 0 = 0 
sumaImapres x = (2*x - 1) + sumaImapres (x - 1) -- siempre dara un numero impar 2x -1, y se repite en bucle restandole 1 a x hasta que de 0

-- 5 
medioFact :: Integer -> Integer 
medioFact x | x == 0 = 1
            | x == 1 = 1
            | otherwise = x * medioFact (x-2) -- es el medio factorial de un numero, multiplica a ese numero y a los que le siguen hasta llegar a 1 saltando de 2 en 2

-- 6
todosDigitosIguales :: Integer ->Bool
todosDigitosIguales x | x < 10 = True 
                      | mod x 10 == mod (div x 10) 10 = todosDigitosIguales (div x 10)
                      | otherwise = False 

--7 
cantDigitos :: Integer -> Integer 
cantDigitos x | x < 10 = 1
              | otherwise = 1 + cantDigitos (div x 10)

iesimoDigito :: Integer ->Integer ->Integer
iesimoDigito x i | i == cantDigitos x = mod x 10
                 | otherwise = iesimoDigito (div x 10) i

--8
sumaDigitos :: Integer ->Integer
sumaDigitos x | x == 0 = 0
              | otherwise = mod x 10 + sumaDigitos (div x 10)

--9 
esCapicua :: Integer ->Bool
esCapicua x | x == x = True

-- 11a
eAprox :: Integer -> Float
eAprox x | x == 0 = 1 
         | otherwise =  1 / fromIntegral (factorial x) + eAprox (x-1) -- fromIntegral, sirve para cambiar un numero Int a uno Float

factorial :: Integer -> Integer
factorial 0 = 1
factorial x = x * (factorial (x-1)) -- hay que poner el (x-1) en parantesis ya que si no lo ponemos entra en un bucle infinito ientras resta al resultado 1 

--12
sucesion :: Integer ->Float
sucesion 1 = 2 
sucesion x = 2 + (1 /sucesion (x-1))

raizDe2Aprox :: Integer ->Float
raizDe2Aprox x = sucesion x - 1 

raizDe2Aprox2 :: Integer -> Float
raizDe2Aprox2 1 = 1
raizDe2Aprox2 n = 1 + (1 / (raizDe2Aprox (n - 1) + 1))

-- 13
sumatoriaInterna :: Integer -> Integer -> Integer
sumatoriaInterna n 1 = n
sumatoriaInterna n m = n^m + sumatoriaInterna n (m-1)

sumatoriaDoble :: Integer -> Integer -> Integer
sumatoriaDoble 1 m = sumatoriaInterna 1 m
sumatoriaDoble n m = sumatoriaInterna n m + sumatoriaDoble (n-1) m 


--14
sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q n 1 = sumaPotenciasA q n 1
sumaPotencias q n m =  sumaPotenciasA q n m  + sumaPotencias q n (m-1)

sumaPotenciasA :: Integer -> Integer -> Integer -> Integer 
sumaPotenciasA q 1 b = q^(b+1)
sumaPotenciasA q a b = q^( a + b ) + sumaPotenciasA q (a-1) b 

--15
sumaRacionalesInterna :: Integer -> Integer -> Float -- aca n queda fijo y sumoma todo los valores de lo pedido miaentras m varia
sumaRacionalesInterna n 1 =  fromIntegral n / 1
sumaRacionalesInterna n m =  fromIntegral n/ fromIntegral m+ sumaRacionalesInterna n (m-1) 

sumaRacionales :: Integer -> Integer -> Float
sumaRacionales 1 m = sumaRacionalesInterna 1 m -- aca cuando n llegue a uno va a saltar a la segunda funcion
sumaRacionales n m = sumaRacionalesInterna n m + sumaRacionales (n-1) m  -- en el primer termino llama a las valores normales y en el segundo termino proero llama a su misma fincion donde n va cambiendo y luego se pasa a la segunda funcion



-- 16a
menorDivisor :: Integer -> Integer
menorDivisor x = numeroDivisor x 2 

numeroDivisor :: Integer -> Integer -> Integer
numeroDivisor x d | mod x d == 0 = d
                  | otherwise = numeroDivisor x (d + 1)

--16b
esPrimo :: Integer -> Bool
esPrimo 1 = False
esPrimo x = menorDivisor x == x --esto funciona por que el menor divisor de un numero primo es si mismo 

--16c

maxComunDivisor :: Int -> Int -> Int --MCD
maxComunDivisor x y | y == 0 = abs x
                    | otherwise = maxComunDivisor y (mod x y) -- cuando vuelve a usar la funcion el valor de y pasa a ser el de x y  el res de mod x y pasa a ser el valor de y

sonCoprimos :: Int -> Int -> Bool
sonCoprimos n m | n == 0 && m == 0 = False
                | otherwise = maxComunDivisor (abs n) (abs m) == 1 --para que dos numerp sean coprimo el MCD entre los dos debe ser uno 

--16d
nEsimoPrimo :: Integer ->Integer -- n-esimo primo (n ≥ 1). Recordar que el primer primo es el 2, el segundo es el 3, 
nEsimoPrimo n = buscarPrimo n 2

buscarPrimo :: Integer -> Integer -> Integer
buscarPrimo 1 x | esPrimo x = x
                | otherwise = buscarPrimo 1 (x + 1)

buscarPrimo n x | esPrimo x = buscarPrimo (n - 1) (x + 1) -- si x es primo entonces pamos a buscar al otro, n es el indice y restamos para buscar cada vez otro primo 
                | otherwise = buscarPrimo n (x + 1) -- va a ir a esta linea cuando x no se primo y querramos seguir buscando el siguiente primo pero probando otra x 


--17
esFibonacci :: Integer -> Bool
esFibonacci x = calculoAux x 0
    
calculoAux  :: Integer -> Integer -> Bool
calculoAux x n | x == fibonacci (n+1) = True
               | x < fibonacci (n+1) = False
               | otherwise = calculoAux x (n+1)

--18
mayorDigitoPar :: Int -> Int
mayorDigitoPar n | n < 10 && par n = n
                 | n < 10 = -1                                                     
                 | par ultimoDigito = max ultimoDigito (mayorDigitoPar (div n 10))                                                             
                 | otherwise = mayorDigitoPar (div n 10)
                  where 
                    ultimoDigito = mod n 10
                    par x = mod x 2 == 0 -- si esto no se cumple va al otherwise

--mayorDigitoPar 583274
--max 4 (mayorDigitoPar 58327)
--max 4 (mayorDigitoPar 5832)
--max 4 (max 2 (mayorDigitoPar 583))
--max 4 (max 2 (mayorDigitoPar 58))
--max 4 (max 2 (max 8 (mayorDigitoPar 5)))
--max 4 (max 2 (max 8 (-1)))
--max 4 (max 2 8)
--max 4 8
--8

--19 (true ↔ n es igual a la suma de los m primeros n´umeros primos, para alg´un m)
esSumaInicialDePrimos :: Integer ->Bool
esSumaInicialDePrimos n | n == 2 = True
                        | n == sumatoriaMprimos 2 n = True
                        | otherwise = False

sumatoriaMprimos :: Integer -> Integer -> Integer
sumatoriaMprimos m n | n <= 0 = 0
                     | esPrimo m = m + sumatoriaMprimos (m+1) (n-m)
                     | otherwise = 0 + sumatoriaMprimos (m+1) n

--20
tomaValorMax :: Integer -> Integer -> Integer
tomaValorMax n1 n2
    | n1 == n2 = n1
    | sumaDivisores n1 >= sumaDivisores (tomaValorMax (n1 + 1) n2) = n1
    | otherwise = tomaValorMax (n1 + 1) n2


sumaDivisores :: Integer -> Integer
sumaDivisores n = sumaDivisoresDesde n 1

sumaDivisoresDesde :: Integer -> Integer -> Integer
sumaDivisoresDesde n d
    | d > n = 0
    | mod n d == 0 = d + sumaDivisoresDesde n (d + 1)
    | otherwise = sumaDivisoresDesde n (d + 1)


--21
pitagoras :: Integer -> Integer -> Integer -> Integer 
pitagoras 0 n r = contarPares 0 n r
pitagoras m n r = contarPares m n r + pitagoras (m-1) n r

contarPares :: Integer -> Integer -> Integer -> Integer
contarPares p 0 r | p^2 <= r^2 = 1
                  | otherwise = 0
contarPares p q r | p^2 + q^2 <= r^2 = 1 + contarPares p (q-1) r
                  |otherwise = contarPares p (q-1) r