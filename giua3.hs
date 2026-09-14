-- 1a
f :: Int -> Int
f(1) = 8
f(4) = 131
f(16) =  16 

-- 1b
g :: Int -> Int
g(8) = 16
g(14) =  4
g(131) = 1

-- 1c (A partir de las funciones definidas en los Items a) y b), implementar las funciones parciales h = f o g y k = g o f)
h :: Int -> Int
h x = f(g x)

k :: Int -> Int
k x = g (f x)

-- 2a (calcular valor absoluto de un numero)
absoluto :: Int -> Int 
absoluto n |n >= 0 = n
            | n < 0 = -n
--           |otherwise = -n (es lo mismo que el anterior) 

-- 2b (devuelve el m´aximo entre el valor absoluto de dos n´umeros enteros)
maximoAbsoluto :: Int -> Int -> Int -- tiene que haber tres Int por que lo primero dos son de los parametrso que le damos y el ultimo del resultado que nos devuelve
maximoAbsoluto n m = max (absoluto n) (absoluto m)  -- se escribe (absoluto a) y no absoluto (a)

-- 2c (devuelve el maximo entre tres n´umeros enteros)
maximo3 :: Int -> Int -> Int -> Int
maximo3 a b c 
    | a >= b && a >= c = a -- && es un and
    | b >= a && b >= c = b 
    | otherwise = c -- cuando pones otherwise hay que poner el igual = y no solo el resultado

-- 2d (dados dos n´umeros racionales, decide si alguno es igual a 0 (resolverlo con y sin pattern matching))
algunoEsCero :: Float -> Float -> Bool
algunoEsCero x y 
        | x == 0  = True
        | y == 0 = True
        | otherwise = False

algunoEsCero2 0 y = True -- esto seria sin usar patter matching
algunoEsCero2 x 0 = True
algunoEsCero2 x y = False

-- 2e (dados dos n´umeros racionales, decide si ambos son iguales a 0 (resolverlo con y sin pattern matching))
ambosSonCero :: Float -> Float -> Bool
ambosSonCero x y 
        | x == 0 && y == 0 = True 
        | otherwise = False 

ambosSonCero2 0 0 = True
ambosSonCero2 0 y = False 
ambosSonCero2 x 0 = False

-- 2f (dados dos numeros reales, indica si estan relacionados por la relacion de equivalencia en R cuyas 
-- clases de equivalencia son: (−∞, 3],(3, 7] y (7, ∞), o dicho de otra manera, si pertenecen al mismo intervalo.)
-- (−∞, 3] = a todo los numeros menor o iguals a 3, (7, ∞) = mayores a siete, (3, 7] = mayores a tres o menores o iguales a 7
enMismoIntervalo :: Int -> Int -> Bool 
enMismoIntervalo a b | a <= 3 && b <= 3 = True --(−∞, 3]
                        | 3 < a && a <= 7 && 3 < b && b <= 7 = True --(3, 7]
                        | a > 7 && b > 7 = True --(7, ∞)
                        | otherwise = False 

-- 2g (que dados tres n´umeros enteros calcule la suma sin sumar repetidos (si los hubiera))
sumaDistintos :: Int -> Int -> Int -> Int
sumaDistintos a b c | a /= b && b /= c && c /=a = a + b + c
                    | a == b &&  b /= c = b + c 
                    | a /= b && b == c = a + b 
                    | a/= b && a == c = b + c
                    | a == b && b == c = a -- otherwise = a (otra forma de escibirlo)
 
 -- 2h (dados dos numeros naturales, decide si el primero es multiplo del segundo)
esMultiploDe :: Int -> Int -> Bool
esMultiploDe a b = mod a b == 0 -- mod se usa para referirse al resto de la divicion, 
                                -- Aca estamo diciendo que el resto entre a y b tiene que ser cero para ser verdadero
                                -- a dividido b 

-- 2i(dado un numero entero, extrae su dıgito de las unidades)
digitoUnidades :: Int -> Int
digitoUnidades a = mod a 10 --mod se queda con el resto, y div con las veces que entra

-- 2j (dado un n´umero entero mayor a 9, extrae su dıgito de las decenas)
digitoDecenas :: Int -> Int
digitoDecenas a | a > 9 = mod a  10

sacarunidades :: Int -> Int
sacarunidades x = div x 10

digitoDecenas2 :: Int -> Int
digitoDecenas2 x = digitoUnidades (sacarunidades x)

-- 3
estanRelacionados :: Int -> Int -> Bool
estanRelacionados a b | a == 0 || b == 0 || div (-a) b == 0 = False -- si la divicion da 0 entonces no existe ningun k 
                      | a*a + a*b*div (-a) b == 0 = True  -- habia que calcular a * a + a * b * k = 0, enonces para saber k, factorizamos y despejamos, 
                                                          --primero: a * (a + b * k) = 0, como la a fuera del parentesis no puede ser cero, despejamos la ecuecion del parentesis
                                                          --  a + b * k = 0, k = -a / b
                      | otherwise = False


-- 4i (Reescribir los ejercicios productoInterno, esParMenor y distancia usando el siguiente renombre de tipos
-- Por lo que entiendo despues del type pones un RENOMBRE referido a algo, en este caso enves de poner (Float, Float) ponemos directamente Punto2D 
-- el renombre simpre tiene que empezar con mayusucla 
type Punto2D = (Float, Float)

-- 4a (calcula el producto interno entre dos tuplas de R × R), el prducto ineternos es: (x, y)* (w, z) = x * w + y * z 
productoInterno ::  Punto2D -> Punto2D -> Float
productoInterno (x,y) (w,z) = x * w + y * z

-- 4b (dadas dos tuplas de R × R, decide si cada coordenada de la primera tupla es menor a la coordenada correspondiente de la segunda tupla.)
esParMenor :: (Float, Float) -> (Float, Float) -> Bool
esParMenor (x,y) (w,z) | x < w && y < z = True  -- otra formaserias: esParMenor (a,b)(c,d) = a < c && b < d (pattern matching)
                        |otherwise = False

esParMenor2 :: (Float, Float) -> (Float, Float) -> (Bool, Bool)
esParMenor2 (x,y) (z,w) = ( x < z , y < w )

-- 4c ( calcula la distancia euclıdea entre dos puntos de R2),la distancia de calculac como raiz (x - w)^2 + (y - z)^2 
distancia :: (Float, Float) -> (Float, Float) -> Float
distancia (x,y) (w,z) = sqrt ((x - w)^2 + (y - z)^2) -- "sqrt" se una para sacar la raiz cudrada, se puede usar solo con Float o Double, NO con Int
                                                    -- para otras raices esdiferetne:  raíz cúbica = elevar a 1/3
                                                    -- elever se puede escribir tanto con ^ o **

-- 4d ( dada una terna de enteros, calcula la suma de sus tres elementos) Terna = (a, b, c)
sumaTerna :: (Int, Int, Int) -> Int
sumaTerna (x,y,z) = x + y + z

-- 4e (dada una terna de n´umeros enteros y un natural, calcula la suma de los elementos de la terna queson multiplos del numero natural)
sumarSoloMultiplos :: (Int, Int, Int) -> Int -> Int
sumarSoloMultiplos (x,y,z) k | mod x k == 0 && mod y k == 0 && mod z k == 0 = x + y + z
                                | mod x k /= 0 && mod y k == 0 && mod z k == 0 =  y + z
                                | mod x k == 0 && mod y k /= 0 && mod z k == 0 =  x + z
                                | mod x k == 0 && mod y k == 0 && mod z k /= 0 =  x + y
                                | mod x k /= 0 && mod y k /= 0 && mod z k == 0 =  z
                                | mod x k /= 0 && mod y k == 0 && mod z k /= 0 =  y
                                | mod x k == 0 && mod y k /= 0 && mod z k /= 0 =  x 
                                |otherwise = 0

-- 4f ( dada una terna de enteros, devuelve la posicion del PRIMER numero par si es que hay alguno, o devuelve 4 si son todos impares)
posPrimerPar :: (Int, Int, Int) -> Int
posPrimerPar (x,y,z) | mod x 2 == 0 = 1 -- para saber si un numero es par tambien se puede usa el "even x"
                     | mod y 2 == 0 = 2 -- Aunque se cumplan que los tres enteros sean pares no va a devolver 1 2 3, si no que solo va a devolver 1 
                     | mod z 2 == 0 = 3 -- ya que al cumplirse la primer linea no sigue keyendo el resto
                     |otherwise = 4

-- 4g (a partir de dos componentes, crea un par con esos valores. Debe funcionar para elementos de cualquier tipo.)
crearPar :: a -> b -> (a, b) -- al poner a y b en ves de Int u otras, podemso introducir un elemento de cualuqier tipoy no solo numeros
crearPar a b = (a, b)

-- 4h ( invierte los elementos del par pasado como par´ametro. Debe funcionar para elementos de cualquier tipo.)
invertir :: (a, b) -> (b, a)   -- no especificmaos el valor de los elementos ya que pueden ser cualquiera      
invertir (a, b) = (b, a)   

-- 5
c :: Int -> Int 
c n | n <= 7 = n^2 
    | n > 7 = 2*n -1

v :: Int -> Int
v n | mod n 2 == 0 = div n 2
    | otherwise = 3*n + 1

todosMenores :: (Int, Int, Int) -> Bool
todosMenores (x,y,z) | c(x) > v(x) && c(y) > v(y) && c(z) > v(z) = True
                        | otherwise = False 

-- 6 
type Anio = Integer
type EsBisiesto = Bool

bisiesto :: Anio -> EsBisiesto
bisiesto a | mod a 4 /= 0 = False 
            | mod a 100 == 0  && mod a 400 /= 0 = False
            | otherwise = True


-- 7a
type Punto3D = (Float, Float, Float)
distanciaManhattan :: Punto3D -> Punto3D -> Float
distanciaManhattan (a,b,c) (x,y,z) = abs (a - x) + abs(b - y) + abs (c - z) -- abs se usa para sacar el valor absoluto de un numero

-- 7b usar type Punto3D = (Float, Float, Float)

--8 
sumaUltimosDosDigitos :: Int -> Int
sumaUltimosDosDigitos x =  abs x  `mod`  10  +   (abs x `div`  10) `mod` 10 -- las comillas (` `) de `mod` y `div`, no cambian su funcion              
                                                                           --   si no que se usan para que visualmente se mas facila de leer
sumaUltimoDosDigitos2 :: Int -> Int         -- uso las funciones que ya habia echo que equivalen a las misma cuanta que nos piden
sumaUltimoDosDigitos2 x = digitoUnidades x + digitoDecenas x 

comparar :: Int -> Int -> Int 
comparar a b | sumaUltimosDosDigitos a < sumaUltimosDosDigitos b  = 1
             | sumaUltimosDosDigitos a > sumaUltimosDosDigitos b = -1
             | sumaUltimosDosDigitos a == sumaUltimosDosDigitos b = 0


               