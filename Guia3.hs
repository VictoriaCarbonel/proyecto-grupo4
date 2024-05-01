

doubleMe x = x + x

f :: Integer -> Integer
f 1 = 8
f 4 = 131
f 16 = 16

g :: Integer -> Integer
g 8 = 16 
g 16 = 4
g 131 = 1

fog :: Integer -> Integer
fog x = f(g x)

gof :: Integer -> Integer
gof x = g(f (x))

absoluto :: Integer -> Integer
absoluto x | x > 0 = x
           | x < 0 = -x

maximoabsoluto :: Integer -> Integer -> Integer 
maximoabsoluto x y | (absoluto x) > (absoluto y) = (absoluto x)
                   | (absoluto x) < (absoluto y) = (absoluto y)
                   | otherwise = (absoluto x)

maximo3 :: Integer -> Integer -> Integer -> Integer
maximo3 x y z | x >= y && x >= z = x
              | y >= x && y >= z = y
              | z >= x && z >= y = z 

algunoEs0 :: Float -> Float -> Bool
algunoEs0 x y | x==0 || y==0 = True
              | otherwise = False   

algunoEs0PM :: Float -> Float -> Bool
algunoEs0PM 0 y = True
algunoEs0PM x 0 = True
algunoEs0PM x y = False

ambosSon0 :: Float -> Float -> Bool
ambosSon0 x y | x==0 && y==0 = True
              | otherwise = False

ambosSon0PM :: Float -> Float -> Bool
ambosSon0PM 0 0 = True
ambosSon0PM x 0 = False
ambosSon0PM 0 y = False

mismoIntervalo :: Float -> Float -> Bool
mismoIntervalo x y | x <= 3 && y <= 3 = True
                   | (x>3 && x<=7) && (y>3 && y<=7) = True
                   | x>7 && y>7 = True
                   |otherwise = False

sumaDistintos :: Integer -> Integer -> Integer -> Integer
sumaDistintos x y z | x==y && x==z = 0
                    | x==y = y+z
                    | y==z = z+x
                    | x==z = z+y
                    | otherwise = x+y+z

esMultiploDe :: Int -> Int -> Bool
esMultiploDe x y | mod x y == 0 = True
                 | otherwise = False

digitoUnidades :: Int -> Int 
digitoUnidades x = mod x 10

digitoDecenas :: Int -> Int
digitoDecenas x = mod (div x 10) 10

estanRelacionados :: Integer -> Integer -> Bool
estanRelacionados x y | x==0 && y==0 = False
                      | mod x y ==0 = True 
                      | otherwise = False

prodInt :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
prodInt (a,b) (c,d) = (a*c, b*d)

todoMenor :: (Float, Float) -> (Float, Float) -> Bool
todoMenor (a,b) (c,d) | a<c && b<d = True
                      | otherwise = False

distanciaPuntos :: (Float, Float) -> (Float, Float) -> Float
distanciaPuntos (a,b) (c,d) = sqrt (((c-a)**2) + ((d-b)**2)) 

sumaTerna :: (Integer, Integer, Integer) -> Integer
sumaTerna (x,y,z) = x+y+z

sumarSoloMultiplos :: (Integer, Integer, Integer) -> Integer -> Integer
sumarSoloMultiplos (a,b,c) d | (a `mod` d ==0 ) && (b `mod` d == 0) && (c `mod` d ==0) = a+b+c
                             | (a `mod` d ==0 ) && (b `mod` d == 0) && (c `mod` d /=0) = a+b
                             | (a `mod` d /=0 ) && (b `mod` d == 0) && (c `mod` d ==0) = b+c
                             | (a `mod` d ==0 ) && (b `mod` d /= 0) && (c `mod` d ==0) = a+c
                             | (a `mod` d == 0) = a
                             | (b `mod` d == 0) = b
                             | (c `mod` d == 0) = c
                             |otherwise = 0

posPrimerPar :: (Integer, Integer, Integer) -> Integer
posPrimerPar (a, b, c) | (a `mod` 2 == 0) = 0
                       | (b `mod` 2 == 0) = 1
                       | (c `mod` 2 ==0) = 2
                       | otherwise = 4

creaPar :: (Eq t) => t -> t -> (t,t)
creaPar a b = (a,b)

invertir :: (Eq t) => (t,t) -> (t,t)
invertir (a,b) = (b,a) 

-- Ejercicio 5

todosMenores :: (Integer, Integer, Integer) -> Bool
todosMenores (a,b,c)| (funcionf(a) > funciong(a) ) && (funcionf(b)> funciong(b)) && (funcionf(c)> funciong(c)) = True
                    | otherwise = False 

funcionf :: Integer -> Integer 
funcionf x | (x <= 7) = x^2
           | (x > 7) = 2*x-1

funciong :: Integer -> Integer
funciong x = if (x `mod` 2 ==0) then (x `div` 2) else (3*x + 1)

-- Ejercicio 6

bisiesto :: Integer -> Bool
bisiesto x | (x `mod` 4 /=0) || ((x `mod` 100 ==0) && (x `mod` 400 /=0)) = False
           | otherwise = True

-- Ejercicio 7

distanciaManhattan :: (Float,Float,Float) -> (Float,Float,Float) -> Float
distanciaManhattan (a,b,c) (d,e,f) = abs ((a-d)+(b-e)+(c-f)) 

-- Ejercicio 8
comparar :: Integer -> Integer -> Integer
comparar x y | sumaUltimosDosDigitos(x) < sumaUltimosDosDigitos(y) = 1
             | sumaUltimosDosDigitos(x) > sumaUltimosDosDigitos(y) = -1
             | otherwise = 0

sumaUltimosDosDigitos :: Integer -> Integer
sumaUltimosDosDigitos x = ((x `mod` 10) + ((x `div` 10) `mod` 10))

parteEntera :: Float -> Integer
parteEntera x | x>=0 && x<1 = 0                                      
              | x>(-1) && x<=0 = -1 
              | x>=1 = 1 + parteEntera(x-1)
              | otherwise = (-1) + parteEntera(x-1)

--Ejercicio 6
sumaDigitos :: Integer -> Integer
sumaDigitos x | x<10 = x
              | otherwise = sumaDigitos(mod x 10) + sumaDigitos(div x 10)
