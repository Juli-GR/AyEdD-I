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

--factorial :: Int -> Int           --(reemplazada en ejercicio 6)
--factorial 0 = 1
--factorial n = n * (factorial (n-1))         --Si <0, infinitamente, o uso la de abajo?

--factorial :: Int -> Int
--factorial n | (n == 1) = 1
--            | (n > 1) = n * (factorial (n-1))

promedio :: [Int] -> Int
promedio xs = div (sumatoria xs) (length xs)
--Se usó div porque hace división entera, a diferencia de "/"
--Tambien se puede hacer manualmente con una función
--que antes le reste a la sumatoria el sobrante usando modulo lenght xs.



--EJERCICIO 3-----------------------------------------------

pertenece :: Int -> [Int] -> Bool
pertenece a [] = False
pertenece a (x:xs)  | (a == x) = True
                    | otherwise = (pertenece a xs)



--EJERCICIO 4-----------------------------------------------
--Reemplazadas en ejercicio 12

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
--Se usó even, pero podría haberse definido una función por casos con módulo 2.

hayMultiplo :: Int -> [Int] -> Bool
hayMultiplo n xs = existe' (map (`mod` n) xs) (==0)
--Se usó `mod` que invierte las variables,
--pero se podría haber definido una función para ello.

sumaCuadrados :: Int -> Int
sumaCuadrados n = sumatoria' [0..n-1] (^2)

factorial :: Int -> Int
factorial n = productoria [1..n]

--multiplicaPares :: [Int] -> Int       --(reemplazada en ejercicio 9)
--multiplicaPares xs = productoria' (filter even xs) id



--EJERCICIO 7-----------------------------------------------

--map aplica una función a cada elemento de una lista, y devuelve la nueva lista.
--map succ [1, -4, 6, 2, -8] equivale a [2, -3, 7, 3, -7].

--filter a partir de una lista, devuelve otra solo con los elementos que cumplen cierta condición.
--filter esPositivo [1, -4, 6, 2, -8] equivale a [1, 6, 2].



--EJERCICIO 8-----------------------------------------------

duplica :: [Int] -> [Int]
duplica [] = []
duplica (x:xs) = (2*x) : duplica xs

duplica' :: [Int] -> [Int]
duplica' xs = map (*2) xs



--EJERCICIO 9-----------------------------------------------

sonPares :: [Int] -> [Int]
sonPares [] = []
sonPares (x:xs) | even x = x : sonpares xs
                | otherwise = sonpares xs 

sonPares' :: [Int] -> [Int]
sonPares' xs = filter even xs

multiplicaPares :: [Int] -> Int
multiplicaPares xs = productoria' sonPares(xs) id



--EJERCICIO 10----------------------------------------------

primIgualesA :: Eq (a) => a -> [a] -> [a]
primIgualesA a [] = []
primIgualesA a (x:xs) | (a == x) = (x : (primIgualesA x xs))
                      | otherwise = []

primIgualesA' ::  Eq (a) => a -> [a] -> [a]  
primIgualesA' a xs = takeWhile (== a) xs



--EJERCICIO 11----------------------------------------------

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

--Bien tipados exhaustivos
--a: Recibe una dupla con dos elementos de distinto tipo.
--g: Recibe una funcion (Int -> Int) (que machea con a) y un entero (que machea con b).

--Bien tipados NO exhaustivos
--c: Recibe una lista de duplas y machea a la primera con x y al resto con xs.
---- No cubre el caso donde la lista es vacía.
--d: Recibe una lista de duplas y machea las dos primeras con (x,y) y (a,b) y al resto con xs.
---- No cubre los casos donde la lista tiene menos de 2 elementos.
--e: Recibe una lista de duplas con un entero y cualquier elemento no entero.
---- No cubre los casos donde el entero no es 0.
--h: Igual que g, pero no es exhaustiva ya que el segundo valor solo puede ser 3.

--Mal tipados
--b: Intenta machear una lista de tuplas con una única tupla.
--f: Recibe una lista de duplas con un entero y cualquier elemento no entero
---- e intenta machear este ultimo con 1 que pertenece a la clase Eq
---- pero la definicion no filtra con ninguna clase.
--i: Recibe dos elementos (una función y un entero) e intenta machearlo con 3 elementos.



--EJERCICIO 14----------------------------------------------

--a: se puede y no existe otra porque al no conocer el tipo, no se le pueden aplicar operaciones
fA :: (a, b) -> b
fA (x, y) = y

--El resto no son posibles ya que todas involucran funciones del tipo (a -> b),
--ya sea que el input es de tipo distinto al output o que como parametro tiene una funcion (a -> b).
--No puede existir una funcion que devuelva cualquier otro tipo distinto al inicial.
