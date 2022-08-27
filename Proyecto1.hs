--Nombre Apellido, año, Comisión x

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
factorial n | (n == 1) = 1
            | (n > 1) = n * (factorial (n-1))
            | (n < 1) = 0                         --Si ingresa neg, dejo q de error?

promedio :: [Int] -> Int
promedio xs = div (sumatoria xs) (length xs)        --Podemos usar div? como hago div entera si no?



--EJERCICIO 3-----------------------------------------------

pertenece :: Int -> [Int] -> Bool
pertenece a [] = False
pertenece a (x:xs)  | (a == x) = True
                    | otherwise = (pertenece a xs)



--EJERCICIO 4-----------------------------------------------

paratodo' :: [a] -> (a -> Bool) -> Bool
paratodo' [] f = True
paratodo' (x:xs) f = (f x) && (paratodo' xs f)

existe' :: [a] -> (a -> Bool) -> Bool
existe' [] f = False
existe' (x:xs) f = (f x) || (existe' xs f)

sumatoria' :: [a] -> (a -> Int) -> Int
sumatoria' [] f = 0
sumatoria' (x:xs) f = (f x) + (sumatoria' xs f)

productoria' :: [a] -> (a -> Int) -> Int
productoria' [] f = 1
productoria' (x:xs) f = (f x) * (productoria' xs f)



--EJERCICIO 5-----------------------------------------------

paratodo :: [a] -> (a -> Bool) -> Bool
paratodo xs f = paratodo' xs f                  --Era esta la idea? medio inutil



--EJERCICIO 6-----------------------------------------------

todosPares :: [Int] -> Bool
todosPares xs = paratodo' xs (even)     --even? con mod o div no funciona??

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' (map (`mod` n) xs) (==0)     --Preguntar!!

sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0..n-1] (^2)  -- usar ^ ?? (el rango esta bien)

factorial' :: Int -> Int    --Usa recursion pero no justo acá(?)
factorial' n = productoria [1..n]

multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria (filter even xs)   --usar primera fun productoria no?



--EJERCICIO 7-----------------------------------------------

--map aplica una función a cada elemento de una lista, y devuelve la nueva lista
--map succ [1, -4, 6, 2, -8] equivale a [2, -3, 7, 3, -7]

--filter a partir de una lista, devuelve otra solo con los elementos que cumplen cierta condición
--filter esPositivo [1, -4, 6, 2, -8] equivale a [1, 6, 2]
