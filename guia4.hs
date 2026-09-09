--Ej 1) Fibonacci

--Resolución con guardas
fibonacci :: Integer -> Integer
fibonacci n | n <= 1 = n
            | otherwise = fibonacci (n-1) + fibonacci (n-2)

--Resolución con Pattern Matching
fibonacci2 :: Integer -> Integer
fibonacci2 0 = 0
fibonacci2 1 = 1
fibonacci2 n = fibonacci2 (n-1) + fibonacci2 (n-2)

--Ejemplo de recursión con potencias de diez

sumaPotenciasDe10 :: Integer -> Integer
sumaPotenciasDe10 1 = 10
sumaPotenciasDe10 n = sumaPotenciasDe10 (n-1) + (10^n)

--Ej14)

sumaPotencias :: Integer -> Integer -> Integer -> Integer
sumaPotencias q 1 m = sumarPotenciasConNFijo q 1 m
sumaPotencias q n m = sumaPotencias q (n-1) m + sumarPotenciasConNFijo q n m

sumarPotenciasConNFijo :: Integer -> Integer -> Integer -> Integer
sumarPotenciasConNFijo q n 1 = q ^ (n+1)
sumarPotenciasConNFijo q n m = sumarPotenciasConNFijo q n (m-1) + q ^ (n+m)

--Ej16) calcular el menor divisor (mayor que 1) de un natural n pasado como parámetro

--aca el caso base no es un numero, es una condición

menorDivisor :: Integer -> Integer
menorDivisor n = menorDivisorDesde n 2

menorDivisorDesde :: Integer -> Integer -> Integer
menorDivisorDesde n d | mod n d /= 0 = menorDivisorDesde n (d+1)
                      | mod n d == 0 = d

--Hacer el ejercicio de los numeros primos, el n-ésimo primo y después el ejercicio 19