-- EJERCICIOS NUMEROS ENTEROS
sucesor :: Int -> Int
sucesor x = x + 1

sumar :: Int -> Int -> Int
sumar x y = x + y

divisionYResto :: Int -> Int -> (Int, Int)
divisionYResto x 0 = error "No se puede dividir por cero"
divisionYResto x y = (div x y ,  mod x y)

maxDelPar :: (Int, Int) -> Int
maxDelPar (x,y) = if(x>y)
                    then x
                    else y

{- PUNTO 1.2

1- sumar 5 (maxDelPar (divisionYResto 15 (sucesor 2)))
2- sumar 5 (maxDelPar (divisionYResto 25 (sucesor 4)))
3- sumar 5 (maxDelPar (divisionYResto (sumar 8 2) (sucesor 1)))
4- maxDelPar (divisionYResto (sumar 10 10) (sucesor 1))
-}

-- EJERCICIOS TIPOS ENUMERATIVOS

data Dir = Norte|Este|Sur|Oeste 
  deriving (Show)

opuesto :: Dir -> Dir
opuesto Norte = Sur
opuesto Este = Oeste
opuesto Sur = Norte
opuesto Oeste = Este

iguales :: Dir -> Dir -> Bool
iguales Norte Norte = True
iguales Este Este = True
iguales Sur Sur = True
iguales Oeste Oeste = True
iguales x y = False

siguiente :: Dir -> Dir
siguiente Norte = Este
siguiente Este = Sur
siguiente Sur = Oeste
siguiente Oeste = error "Oeste es la ultima direccion"
-- La precondicion de esta funcion es "no pasar el valor Oeste", y es parcial ya que no acepta todos los posibles valores que puede tomar el argumento Dir. 

data DiaDeSemana = Lunes|Martes|Miercoles|Jueves|Viernes|Sabado|Domingo

primeroYUltimoDia :: (DiaDeSemana,DiaDeSemana)
primeroYUltimoDia = (Lunes,Domingo)

empiezaConM :: DiaDeSemana -> Bool
empiezaConM Martes = True
empiezaConM Miercoles = True
empiezaConM _ = False

numeroDelDiaDeLaSemana :: DiaDeSemana -> Int
numeroDelDiaDeLaSemana Lunes = 1
numeroDelDiaDeLaSemana Martes = 2
numeroDelDiaDeLaSemana Miercoles = 3
numeroDelDiaDeLaSemana Jueves = 4
numeroDelDiaDeLaSemana Viernes = 5
numeroDelDiaDeLaSemana Sabado = 6
numeroDelDiaDeLaSemana Domingo = 7

vieneDespues :: DiaDeSemana -> DiaDeSemana -> Bool
vieneDespues x y = numeroDelDiaDeLaSemana x > numeroDelDiaDeLaSemana y

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

negar :: Bool -> Bool
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True v = v
implica False _ = True

yTambien :: Bool -> Bool -> Bool
yTambien True v = v
yTambien False _ = False

oBien :: Bool -> Bool -> Bool
oBien True _ = True
oBien _ v = v

-- EJERCICIOS REGISTROS

data Persona = P String Int deriving (Show)
--               Nombre Edad

nombre :: Persona -> String
nombre (P n _) = n 

edad :: Persona -> Int
edad (P _ e) = e

crecer :: Persona -> Persona
crecer (P n e) = P n (e + 1)

cambioDeNombre :: String -> Persona -> Persona
cambioDeNombre nn (P n e) = P nn e

esMayorQueLaOtra :: Persona -> Persona -> Bool
esMayorQueLaOtra (P n e) (P n2 e2) = e > e2

laQueEsMayor :: Persona -> Persona -> Persona
laQueEsMayor p1 p2 = if esMayorQueLaOtra p1 p2
                                    then p1
                                    else p2

data TipoDePokemon = Agua | Fuego | Planta
    deriving Show

data Pokemon = Po TipoDePokemon Int
--                              porcentaje de energia
    deriving Show

data Entrenador = E String Pokemon Pokemon
--                  Nombre
    deriving Show

superaA :: Pokemon -> Pokemon -> Bool
superaA (Po t1 _) (Po t2 _) = esteTipoDePokemon_SuperaAEste_ t1 t2

esteTipoDePokemon_SuperaAEste_ :: TipoDePokemon -> TipoDePokemon -> Bool
esteTipoDePokemon_SuperaAEste_ Agua Fuego = True
esteTipoDePokemon_SuperaAEste_ Fuego Planta = True
esteTipoDePokemon_SuperaAEste_ Planta Agua = True
esteTipoDePokemon_SuperaAEste_ _ _ = False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe x (E _ p1  p2) = (unoSiCeroSino (pokemonEsDeTipo p1 x)) + (unoSiCeroSino (pokemonEsDeTipo p2 x))

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino _ = 0

sonElMismoTipoDePokemon :: TipoDePokemon -> TipoDePokemon -> Bool
sonElMismoTipoDePokemon Fuego Fuego = True
sonElMismoTipoDePokemon Agua Agua = True
sonElMismoTipoDePokemon Planta Planta = True
sonElMismoTipoDePokemon _ _ = False

pokemonEsDeTipo :: Pokemon -> TipoDePokemon -> Bool
pokemonEsDeTipo p y = sonElMismoTipoDePokemon (tipoDePokemon p) y

tipoDePokemon :: Pokemon -> TipoDePokemon
tipoDePokemon (Po x _) = x

juntarPokemon :: (Entrenador,Entrenador) -> [Pokemon]
juntarPokemon (e1, e2) = pokemonesDe e1 ++ pokemonesDe e2

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (E _ x y) = [x,y]

ash :: Entrenador
ash = (E "Ash" pikachu charmander)

pikachu :: Pokemon 
pikachu = (Po Planta 100) 

charmander :: Pokemon
charmander = (Po Planta 10)                                 

medina :: Entrenador
medina = (E "Medina" pikachu charmander)

--EJERCICIOS FUNCIONES POLIMORFICAS

loMismo :: a -> a
loMismo x = x

siempreSiete :: a -> Int
siempreSiete x = 7

swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)
--Porque las variables pueden ser diferente tipo

-- 2) Son polimorficas porque le podes pasar expresiones de cualquier tipo y van a funcionar de todas formas

--EJERCICIOS PATTERN MATCHING SOBRE LISTAS

estaVacia :: [a] -> Bool
estaVacia [] = True
estaVacia _ = False

elPrimero :: [a] -> a
--PRECONDICIONES: No puede recibir una lista vacia.
elPrimero (x:_) = x
elPrimero [] = error "No se puede pedir el primer elemento de una lista vacia"

sinElPrimero :: [a] -> [a]
--PRECONDICIONES: No puede recibir una lista vacia.
sinElPrimero (x : xs) = xs
sinElPrimero [] = error "No se puede pedir el resto a una lista vacia"

splitHead :: [a] -> (a, [a])
--PRECONDICIONES: No puede recibir una lista vacia.
splitHead (x:xs) = (x,xs)
splitHead [] = error "No se puede pedir un splitHead a una lista vacia"

