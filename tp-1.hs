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
vieneDespues x y = if(numeroDelDiaDeLaSemana x > numeroDelDiaDeLaSemana y)
                    then True
                    else False

estaEnElMedio :: DiaDeSemana -> Bool
estaEnElMedio Lunes = False
estaEnElMedio Domingo = False
estaEnElMedio _ = True

negar :: Bool -> Bool
negar True = False
negar False = True

implica :: Bool -> Bool -> Bool
implica True False = False
implica _ _ = True

yTambien :: Bool -> Bool -> Bool
yTambien True True = True
yTambien _ _ = False

oBien :: Bool -> Bool -> Bool
oBien False False = False
oBien _ _ = True

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
laQueEsMayor (P n e) (P n2 e2) = if(esMayorQueLaOtra (P n e) (P n2 e2))
                                    then P n e
                                    else P n2 e2

data TipoDePokemon = Agua | Fuego | Planta

data Pokemon = Po TipoDePokemon Int
--                              porcentaje de energia

data Entrenador = E String Pokemon Pokemon
--                  Nombre

superaA :: Pokemon -> Pokemon -> Bool
superaA (Po Agua _) (Po Fuego _) = True
superaA (Po Fuego _) (Po Planta _) = True
superaA (Po Planta _) (Po Agua _) = True
superaA _ _ = False

cantidadDePokemonDe :: TipoDePokemon -> Entrenador -> Int
cantidadDePokemonDe x (E _ p1  p2) = siLosTiposSonIgualesSuma1 x (elPokemonEsDeTipo p1) + siLosTiposSonIgualesSuma1 x (elPokemonEsDeTipo p2)

siLosTiposSonIgualesSuma1 :: TipoDePokemon -> TipoDePokemon -> Int
siLosTiposSonIgualesSuma1 Fuego Fuego = 1
siLosTiposSonIgualesSuma1 Agua Agua = 1
siLosTiposSonIgualesSuma1 Planta Planta = 1
siLosTiposSonIgualesSuma1 _ _ = 0

elPokemonEsDeTipo :: Pokemon -> TipoDePokemon
elPokemonEsDeTipo (Po x _) = x

juntarPokemon :: (Entrenador,Entrenador) -> [Pokemon]
juntarPokemon ((E _ x y),(E _ n m)) = x:y:n:m:[]

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
elPrimero (x:_) = x
elPrimero [] = error "No se puede pedir el primer elemento de una lista vacia"

sinElPrimero :: [a] -> [a]
sinElPrimero (x : xs) = xs
sinElPrimero [] = error "No se puede pedir el resto a una lista vacia"

splitHead :: [a] -> (a, [a])
splitHead (x:xs) = (x,xs)
splitHead [] = error "No se puede pedir un splitHead a una lista vacia"

