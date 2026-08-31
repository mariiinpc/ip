absoluto1:: Int -> Int
absoluto1 x | x < 0 = x * (-1)
            | otherwise = x

digitoUnidades:: Int -> Int
digitoUnidades x = mod (absoluto1 x) 10

digitoDecenas:: Int -> Int
digitoDecenas x = digitoUnidades (sacarUnidades x)

sacarUnidades:: Int -> Int
sacarUnidades x= div x 10

esParMenor1:: (Float,Float) -> (Float,Float) -> Bool
esParMenor1 (x,y) (a,b) | (x,y) < (a,b) = True
                        | otherwise = False

posPrimerPar:: (Int,Int,Int) -> Int
posPrimerPar (a,b,c) | mod a 2 == 0 = 0
                     | mod b 2 == 0 = 1
                     | mod c 2 == 0 = 2
                     | otherwise = 4

type Coordenada3D = (Float,Float,Float)

distanciaManhattan:: Coordenada3D -> Coordenada3D -> Float
distanciaManhattan (a,b,c) (d,e,f) = absoluto (a - d) + absoluto (b - e) + absoluto (c - f)

absoluto:: Float -> Float
absoluto x | x < 0 = x * (-1)
           | otherwise  = x

sumaUlt2Digitos:: Int ->Int
sumaUlt2Digitos x = digitoUnidades x + digitoDecenas x