--Nombre Apellido, año, Comisión x

--El enunciado dice q se usa aplicación parcial, cuando?

--EJERCICIO 1-----------------------------------------------

esCero :: Int -> Bool
esCero x = x == 0

esPositivo :: Int -> Bool
esPositivo x = x > 0

esVocal :: Char -> Bool
esVocal x =
    x == 'a' || x == 'e' || x == 'i' || x == 'o' || x == 'u'



--EJERCICIO 2-----------------------------------------------

--paratodo :: [Bool] -> Bool        --(reemplazada por ejercicio 5)
--paratodo [] = True
--paratodo (x:xs) = x && (paratodo xs)

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + (sumatoria xs)

productoria :: [Int] -> Int
productoria [] = 1
productoria (x:xs) = x * (productoria xs)

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * (factorial (n-1))         --Si <0, infinitamente, o uso la de abajo?

--factorial :: Int -> Int
--factorial n | (n == 1) = 1
--            | (n > 1) = n * (factorial (n-1))

promedio :: [Int] -> Int
promedio xs = div (sumatoria xs) (length xs)
--Usé div porque hace división entera, a diferencia de "/"
--Tambien se puede hacer manualmente con una función
--que antes le reste a la sumatoria el sobrante usando modulo lenght xs



--EJERCICIO 3-----------------------------------------------

pertenece :: Int -> [Int] -> Bool
pertenece a [] = False
pertenece a (x:xs)  | (a == x) = True
                    | otherwise = (pertenece a xs)



--EJERCICIO 4-----------------------------------------------


--paratodo' :: [a] -> (a -> Bool) -> Bool
--paratodo' [] f = True
--paratodo' (x:xs) f = (f x) && (paratodo' xs f)

--existe' :: [a] -> (a -> Bool) -> Bool
--existe' [] f = False
--existe' (x:xs) f = (f x) || (existe' xs f)

--sumatoria' :: [a] -> (a -> Int) -> Int
--sumatoria' [] f = 0
--sumatoria' (x:xs) f = (f x) + (sumatoria' xs f)

--productoria' :: [a] -> (a -> Int) -> Int
--productoria' [] f = 1
--productoria' (x:xs) f = (f x) * (productoria' xs f)



--EJERCICIO 5-----------------------------------------------

paratodo :: [Bool] -> Bool
paratodo xs = paratodo' xs id



--EJERCICIO 6-----------------------------------------------

todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs (even)
--Se usó even, pero podría haberse definido una función por casos con módulo 2

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' (map (`mod` n) xs) (==0)
--Se usó `mod` que invierte las variables,
--pero se podría haber definido una función para ello

sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0..n-1] (^2)

factorial' :: Int -> Int    --Usa recursion pero no justo acá(?)
factorial' n = productoria [1..n]

multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria (filter even xs)   --usar primera fun productoria no?



--EJERCICIO 7-----------------------------------------------

--map aplica una función a cada elemento de una lista, y devuelve la nueva lista
--map succ [1, -4, 6, 2, -8] equivale a [2, -3, 7, 3, -7]

--filter a partir de una lista, devuelve otra solo con los elementos que cumplen cierta condición
--filter esPositivo [1, -4, 6, 2, -8] equivale a [1, 6, 2]



--EJERCICIO 8-----------------------------------------------

duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = (2*x) : duplica xs

duplica' :: [Int] -> [Int]
duplica' xs = map (*2) xs



--EJERCICIO 9-----------------------------------------------

sonpares :: [Int] -> [Int]
sonpares [] = []
sonpares (x:xs) | mod x 2 == 0 = x : sonpares xs
                | otherwise = sonpares xs 

sonpares' :: [Int] -> [Int]
sonpares' xs = filter even xs



--EJERCICIO 10----------------------------------------------

primIgualesA :: Eq (a) => a -> [a] -> [a]
primIgualesA a [] = []
primIgualesA a (x:xs) | (a == x) = (x : (primIgualesA x xs))
                      | otherwise = []

primIgualesA' ::  Eq (a) => a -> [a] -> [a]  
primIgualesA' a xs = takeWhile (== a) xs



--EJERCICIO 11----------------------------------------------

--primIguales :: Eq (a) => [a] -> [a]
--primIguales [] = []
--primIguales (x:[]) = x : primIguales []
--primIguales (x:y:xs)  x == y = x : primIguales (y:xs)

primIguales :: Eq (a) => [a] -> [a]
primIguales [] = []
primIguales (x:xs) | (xs!!0 == x) = x : primIguales xs 
                   | otherwise = [x]

primIguales' :: Eq (a) => [a] -> [a]
primIguales' xs = primIgualesA' (xs!!0) xs



--EJERCICIO 12----------------------------------------------

cuantGen :: (b -> b -> b) -> b -> [a] -> (a -> b) -> b
cuantGen op z [] t = z
cuantGen op z (x:xs) t = op (t x) (cuantGen op z xs t)


paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' xs f = cuantGen (&&) (True) xs f 

existe' :: [a] -> (a -> Bool) -> Bool
existe' xs f = cuantGen (||) (False) xs f 

sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' xs f = cuantGen (+) (0) xs f 

productoria' :: [a] -> (a -> Int) -> Int
productoria' xs f = cuantGen (*) (1) xs f 



--EJERCICIO 13----------------------------------------------

--a, bien
--b, mal, ((a,b):xs)
--c, bien
--d, non exhaustive patterns
--e, non exhaustive patterns
--f

f :: [(Int,a)] -> Int
f [(0,a)] = 3



--EJERCICIO 14----------------------------------------------
--aparte de la a, ninguna se puede porq no existen funciones q tienen cualquier input y el output es cualquiera pero distinto
