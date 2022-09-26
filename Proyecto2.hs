data Carrera = Matematica | Fisica | Computacion | Astronomia
    deriving (Eq, Show)

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

minimoElemento' :: (Bounded a, Ord a) => [a] -> a --como usa bounded, especificar tipo [1,2] ::Int
minimoElemento' [] = maxBound
minimoElemento' (x:xs) = min x (minimoElemento xs)

masGrave :: [NotaBasica] -> NotaBasica
masGrave xs = minimoElemento' xs


--Ejercicio 4-----------------------------------------------------

type Ingreso = Int

--tipo enumerado
data Cargo = Titular | Asociado | Adjunto | Asistente | Auxiliar
    deriving (Eq, Show)
data Area = Administrativa | Ensenanza | Economica | Postgrado
    deriving (Eq, Show)

data Persona = Decane   --tipo algebraico
            |Docente Cargo
            |NoDocente Area
            |Estudiante Carrera Ingreso
    deriving (Eq, Show)


--tipo de docente   Docente :: Cargo -> Persona

--un quilombo pero no era ESTRICTAMENTE necesario el eq(???
-- si en cuantos_doc' lo uso, acá tmb???
cuantos_doc :: [Persona] -> Cargo -> Int
cuantos_doc [] c = 0
cuantos_doc ((Docente Titular):xs) Titular = 1 + (cuantos_doc xs Titular)
cuantos_doc ((Docente Asociado):xs) Asociado = 1 + (cuantos_doc xs Asociado)
cuantos_doc ((Docente Adjunto):xs) Adjunto = 1 + (cuantos_doc xs Adjunto)
cuantos_doc ((Docente Asistente):xs) Asistente = 1 + (cuantos_doc xs Asistente)
cuantos_doc ((Docente Auxiliar):xs) Auxiliar = 1 + (cuantos_doc xs Auxiliar)
cuantos_doc (x:xs) c = (cuantos_doc xs c)

--cuantos_doc :: [Persona] -> Cargo -> Int
--cuantos_doc [] c = 0
--cuantos_doc (x:xs) c
--    |x == (Docente c) = 1 + (cuantos_doc xs c)
--    |otherwise = (cuantos_doc xs c)

--Lo hago así???
--Tecnicamente puedo definir una funcion aparte que me lo haga sin Eq pero quilombo
cuantos_doc' :: [Persona] -> Cargo -> Int
cuantos_doc' [] c = 0
cuantos_doc' xs c = length (filter (== Docente c) xs)


--Ejercicio 5-----------------------------------------------------

data Alteracion = Bemol | Sostenido | Natural

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


--Ejercicio 6-----------------------------------------------------

primerElemento :: [a] -> Maybe a
primerElemento [] = Nothing
primerElemento (x:xs) = Just x


--Ejercicio 7-----------------------------------------------------

data Cola = VaciaC | Encolada Persona Cola

atender :: Cola -> Maybe Cola
atender VaciaC = Nothing
atender (Encolada p c) = Just c

encolar :: Persona -> Cola -> Cola
encolar a VaciaC = (Encolada a) VaciaC
encolar a (Encolada p c) = (Encolada p (encolar a c))

busca :: Cola -> Cargo -> Maybe Persona
busca VaciaC _ = Nothing
busca (Encolada (Docente Titular) x) Titular = Just (Docente Titular)
busca (Encolada (Docente Asociado) x) Asociado = Just (Docente Asociado)
busca (Encolada (Docente Adjunto) x) Adjunto = Just (Docente Adjunto)
busca (Encolada (Docente Asistente) x) Asistente = Just (Docente Asistente)
busca (Encolada (Docente Auxiliar) x) Auxiliar = Just (Docente Auxiliar)
busca (Encolada p x) cargo = busca x cargo

--b) se parece a las listas

--Ejercicio 8-----------------------------------------------------

data ListaAsoc a b = Vacia | Nodo a b ( ListaAsoc a b )
type Diccionario = ListaAsoc String String
type Padron = ListaAsoc Int String

type GuiaTelefonica = ListaAsoc String Int

la_long :: ListaAsoc a b -> Int
la_long Vacia = 0
la_long (Nodo a b xs) = 1 + la_long xs

la_concat :: ListaAsoc a b -> ListaAsoc a b -> ListaAsoc a b
la_concat Vacia ys = ys
la_concat (Nodo a b xs) ys = (Nodo a b (la_concat xs ys))

la_agregar :: ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar la a b = Nodo a b la

--PREGUNTAR si se agrega al principio o al final (la_agregar)

la_agregar' :: ListaAsoc a b -> a -> b -> ListaAsoc a b
la_agregar' Vacia x y = Nodo x y Vacia
la_agregar' (Nodo a b la) x y = Nodo a b (la_agregar' la x y)

la_pares :: ListaAsoc a b -> [(a, b)]
la_pares Vacia = []
la_pares (Nodo a b la) = (a, b):(la_pares la)

la_busca :: Eq a => ListaAsoc a b -> a -> Maybe b
la_busca Vacia _ = Nothing
la_busca (Nodo a b la) x
    |a==x = Just b
    |otherwise = la_busca la x

la_borrar :: Eq a => a -> ListaAsoc a b -> ListaAsoc a b
la_borrar x Vacia = Vacia
la_borrar x (Nodo a b la)
  |x==a = la_borrar x la
  |otherwise = Nodo a b (la_borrar x la)

--PREGUNTA cuando usas case?????? ni lo usé
--tmp uso derivings? o si?


--Ejercicio 9-----------------------------------------------------

data Arbol a = Hoja | Rama (Arbol a) a (Arbol a)

type Prefijos = Arbol String
can, cana, canario, canas, cant, cantar, canto :: Prefijos
can = Rama cana "can" cant
cana = Rama canario "a" canas
canario = Rama Hoja "rio" Hoja
canas = Rama Hoja "s" Hoja
cant = Rama cantar "t" canto
cantar = Rama Hoja "ar" Hoja
canto = Rama Hoja "o" Hoja

a_long :: Arbol a -> Int
a_long Hoja = 0
a_long (Rama a1 a a2) = 1 + (a_long a1) + (a_long a2)

a_hojas :: Arbol a -> Int
a_hojas Hoja = 1
a_hojas (Rama a1 a a2) = (a_hojas a1) + (a_hojas a2)

a_inc :: Num a => Arbol a -> Arbol a
a_inc Hoja = Hoja
a_inc (Rama a1 a a2) = (Rama (a_inc a1) (a+1) (a_inc a2))

a_map :: (a -> b) -> Arbol a -> Arbol b
a_map f Hoja = Hoja
a_map f (Rama a1 a a2) = (Rama (a_map f a1) (f a) (a_map f a2))
