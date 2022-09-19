data Carrera = Matematica | Fisica | Computacion | Astronomia
    deriving Eq

titulo :: Carrera -> String
titulo Matematica = "Licenciatura en Matemática"
titulo Fisica = "Licenciatura en Fisica"
titulo Computacion = "Licenciatura en Ciencias de la Computacion"
titulo Astronomia = "Licenciatura en Astronomia"

data NotaBasica = Do | Re | Mi | Fa | Sol | La | Si
    deriving (Eq, Ord, Show, Bounded)

cifradoAmericano :: NotaBasica -> Char
cifradoAmericano Do = 'C'
cifradoAmericano Re = 'D'
cifradoAmericano Mi = 'E'
cifradoAmericano Fa = 'F'
cifradoAmericano Sol = 'G'
cifradoAmericano La = 'A'
cifradoAmericano Si = 'B'


--Ejercicio 3-----------------------------------------------------

minimoElemento :: Ord a => [a] -> a
minimoElemento [x] = x
minimoElemento (x:xs) = min x (minimoElemento xs)

minimoElemento' :: (Bounded a, Ord a) => [a] -> a   --como usa bounded, especificar tipo [1,2] ::Int
minimoElemento' [] = maxBound
minimoElemento' (x:xs) = min x (minimoElemento xs)

masGrave :: [NotaBasica] -> NotaBasica
masGrave xs = minimoElemento' xs

--Ejercicio 4-----------------------------------------------------

type Ingreso = Int

--tipos enumerados
data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar
    deriving Eq
data Area = Administrativa | Ensenanza | Economica | Postgrado
    deriving Eq

data Persona = Decane   --tipo algebraico
            |Docente Cargo
            |NoDocente Area
            |Estudiante Carrera Ingreso
    deriving Eq


--tipo de docente   Docente :: Cargo -> Persona

cuantos_doc :: [Persona] -> Cargo -> Int
cuantos_doc [] c = 0
cuantos_doc (x:xs) c
    |x == (Docente c) = 1 + (cuantos_doc xs c)
    |otherwise = (cuantos_doc xs c)

cuantos_doc' :: [Persona] -> Cargo -> Int
cuantos_doc' [] c = 0
cuantos_doc' xs c = length (filter (== Docente c) xs)

--Ejercicio 5-----------------------------------------------------






data Alteracion = Bemol | Sostenido | Natural deriving Eq

data NotaMusical = Nota NotaBasica Alteracion 
instance Eq NotaMusical
    where
      nm1 == nm2 = sonidoCromatico nm1 == sonidoCromatico nm2
instance Ord NotaMusical
    where
      nm1 <= nm2 = sonidoCromatico nm1 <= sonidoCromatico nm2

sonido :: NotaBasica -> Int
sonido Do = 1
sonido Re = 3
sonido Mi = 5
sonido Fa = 6
sonido Sol = 8
sonido La = 10
sonido Si = 12

sonidoCromatico :: NotaMusical -> Int
sonidoCromatico (Nota nb Bemol) = (sonido nb) -1
sonidoCromatico (Nota nb Sostenido) = (sonido nb) +1
sonidoCromatico (Nota nb Natural) = (sonido nb)


-------6

primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (x:xs) = Just x


------7
data Cola = VaciaC | Encolada Persona Cola

atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada p x) = Just x

encolar :: Persona -> Cola -> Cola
encolar p c = (Encolada p) c

busca :: Cola -> Cargo -> Maybe Persona
busca VaciaC _ = Nothing
busca (Encolada (Docente c) x) cargo
    |cargo == c = Just (Docente c)
    |otherwise = busca x cargo
busca (Encolada p x) cargo = busca x cargo

--Ver si funciona el 7, ver todos los deriving inutiles

-----8
