-- RECURSION SOBRE LISTAS :

-- PUNTO 1:

sumatoria :: [Int] -> Int
sumatoria [] = 0
sumatoria (x:xs) = x + sumatoria xs

--PUNTO 2:

longitud :: [a] -> Int
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

-- PUNTO 3:

sucesores :: [Int] -> [Int]
sucesores [] = []
sucesores (x:xs) = (x + 1) : sucesores xs

--PUNTO 4:

conjuncion :: [Bool] -> Bool
-- PRECOND: La lista no debe ser una lista vacia
conjuncion [] = True
conjuncion (x:xs) = x && conjuncion xs

-- PUNTO 5:

disyuncion :: [Bool] -> Bool
disyuncion [] = False
disyuncion (x:xs) = x || disyuncion xs

-- PUNTO 6:

aplanar :: [[a]] -> [a]
aplanar [] = []
aplanar (x:xs) = x ++ aplanar xs

-- PUNTO 7:

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ []     = False
pertenece t (x:xs) = (t == x) || pertenece t xs

-- PUNTO 8:

apariciones :: Eq a => a -> [a] -> Int
apariciones _ [] = 0
apariciones x (y:ys) = unoSiCeroSino (x == y) + apariciones x ys

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino _ = 0

-- PUNTO 9:

losMenoresA :: Int -> [Int] -> [Int]
losMenoresA _ [] = []
losMenoresA n (x:xs) = hacerListaConElemento_Si x (x<n) ++ losMenoresA n xs

hacerListaConElemento_Si :: a -> Bool -> [a]
hacerListaConElemento_Si x True = [x]
hacerListaConElemento_Si _ False = []

-- PUNTO 10:

lasDeLongitudMayorA :: Int -> [[a]] -> [[a]]
lasDeLongitudMayorA _ [] = []
lasDeLongitudMayorA n (x:xs) = hacerListaConElemento_Si x ((longitud x) > n) ++ lasDeLongitudMayorA n xs 

-- PUNTO 11:

agregarAlFinal :: [a] -> a -> [a]
agregarAlFinal []     y = (y:[])
agregarAlFinal (x:xs) y = x : agregarAlFinal xs y

-- PUNTO 12:

agregar :: [a] -> [a] -> [a]
agregar []     ys     = ys
agregar (x:xs) ys     = x : agregar xs ys

-- PUNTO 13:

reversa :: [a] -> [a]
reversa []     = []
reversa (x:xs) = reversa xs ++ [x]

-- PUNTO 14:

zipMaximos :: [Int] -> [Int] -> [Int]
zipMaximos []     ys     = ys 
zipMaximos xs     []     = xs
zipMaximos (x:xs) (y:ys) = maxDelPar (x,y) : zipMaximos xs ys

maxDelPar :: (Int, Int) -> Int
maxDelPar (x,y) = if(x>y)
                    then x
                    else y

-- PUNTO 15:

elMinimo :: Ord a => [a] -> a
--PRECOND: La lista no debe ser vacia
elMinimo [x] = error "No puede ser lista vacia"
elMinimo (x:xs) = if (esListaVacia xs)
                    then x
                    else min x (elMinimo xs)

-- RECURSION SOBRE NUMEROS:

-- PUNTO 1:

factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)

-- PUNTO 2:

cuentaRegresiva :: Int -> [Int]
cuentaRegresiva n = if n>=1
                        then n : cuentaRegresiva (n-1)
                        else []

--PUNTO 3:

repetir :: Int -> a -> [a]
repetir 0 x = []
repetir n x = x : repetir (n-1) x

--PUNTO 4:

losPrimeros :: Int -> [a] -> [a]
losPrimeros _ []     = []
losPrimeros 0 _      = []
losPrimeros n (x:xs) = x : losPrimeros (n-1) xs

--PUNTO 5:

sinLosPrimeros :: Int -> [a] -> [a]
sinLosPrimeros _ []     = []
sinLosPrimeros 0 (x:xs) = (x:xs)
sinLosPrimeros n (x:xs) = sinLosPrimeros (n-1) xs

-- REGISTROS:

-- PUNTO 1:

data Persona = P String Int
--               Nombre Edad
    deriving Show

edad :: Persona -> Int
edad (P _ e) = e

francisco :: Persona 
francisco = P "Francisco" 21

marco :: Persona 
marco = P "Marco" 29

elizabeth :: Persona
elizabeth = P "Elizabeth" 52

-- PUNTO 1.a:

mayoresA :: Int -> [Persona] -> [Persona]
mayoresA n []     = []
mayoresA n (p:ps) = if edad p > n
                        then p : mayoresA n ps
                        else mayoresA n ps

listaConElemento_Si_ :: a -> Bool -> [a]
listaConElemento_Si_ x True  = [x]
listaConElemento_Si_ x False = []

-- PUNTO 1.b:

promedioEdad :: [Persona] -> Int
-- PRECOND: La lista no puede ser vacia
promedioEdad ps = div (sumarTodasLasEdadesDe ps) (longitud ps)

sumarTodasLasEdadesDe :: [Persona] -> Int
sumarTodasLasEdadesDe []     = 0
sumarTodasLasEdadesDe (p:ps) = edad p + sumarTodasLasEdadesDe ps

-- PUNTO 1.c:

elMasViejo :: [Persona] -> Persona
-- PRECOND: La lista no puede ser vacia
elMasViejo []        = error "No puede ser vacia"
elMasViejo (p:ps)     = if (esListaVacia ps)
                            then p
                            else elMasViejoSabiendoQueNoEsVacia (p:ps)
                                
elMasViejoSabiendoQueNoEsVacia :: [Persona] -> Persona
elMasViejoSabiendoQueNoEsVacia (p:ps) = if (edad p) > (edad (elMasViejo ps))
                                            then p 
                                            else elMasViejo ps

esListaVacia :: [a] -> Bool
esListaVacia []     = True
esListaVacia (x:xs) = False

--PUNTO 2:

data TipoDePokemon = Agua | Fuego | Planta

data Pokemon = Po TipoDePokemon Int

pikachu :: Pokemon
pikachu = Po Agua 4

garmander :: Pokemon
garmander = Po Planta 7

mon :: Pokemon
mon = Po Fuego 11

data Entrenador = E String [Pokemon]

pokemonesDe :: Entrenador -> [Pokemon]
pokemonesDe (E _ pks) = pks

ash :: Entrenador
ash = E "Ash" [pikachu, garmander]

-- PUNTO 2.a:

cantPokemon :: Entrenador -> Int
cantPokemon (E n ps) = longitud ps

-- PUNTO 2.b:

cantPokemonDe :: TipoDePokemon -> Entrenador -> Int
cantPokemonDe x (E n ps) = cantPokemonDe_EntreEstos_ x ps

cantPokemonDe_EntreEstos_ :: TipoDePokemon -> [Pokemon] -> Int
cantPokemonDe_EntreEstos_ x  []     = 0
cantPokemonDe_EntreEstos_ tp (p:ps) = unoSiCeroSino (sonElMismoTipoDePokemon tp (tipoDePokemon p)) + cantPokemonDe_EntreEstos_ tp ps

tipoDePokemon :: Pokemon -> TipoDePokemon
tipoDePokemon (Po x _) = x

sonElMismoTipoDePokemon :: TipoDePokemon -> TipoDePokemon -> Bool
sonElMismoTipoDePokemon Fuego Fuego = True
sonElMismoTipoDePokemon Agua Agua = True
sonElMismoTipoDePokemon Planta Planta = True
sonElMismoTipoDePokemon _ _ = False

-- PUNTO 2.c:

cuantosDeTipo_De_LeGananATodosLosDe_ :: TipoDePokemon -> Entrenador -> Entrenador -> Int
cuantosDeTipo_De_LeGananATodosLosDe_ tipo (E _ pokemons1) (E _ pokemons2) = cantPokemonesDeTipoQueLeGananATodos tipo pokemons1 pokemons2

cantPokemonesDeTipoQueLeGananATodos :: TipoDePokemon -> [Pokemon] -> [Pokemon] -> Int
cantPokemonesDeTipoQueLeGananATodos _ [] pokemons2 = 0
cantPokemonesDeTipoQueLeGananATodos tipo (pokemon1: pokemones1) pokemones2 = if sonElMismoTipoDePokemon (tipoDePokemon pokemon1) tipo 
                                                                                then unoSiCeroSino (venceATodos pokemon1 pokemones2) + cantPokemonesDeTipoQueLeGananATodos tipo pokemones1 pokemones2
                                                                                else cantPokemonesDeTipoQueLeGananATodos tipo pokemones1 pokemones2

venceATodos :: Pokemon -> [Pokemon] -> Bool
venceATodos _ [] = True
venceATodos pokemon1 (pokemon2 : pokemones2) = superaA pokemon1 pokemon2 && venceATodos pokemon1 pokemones2

superaA :: Pokemon -> Pokemon -> Bool
superaA (Po t1 _) (Po t2 _) = esteTipoDePokemon_SuperaAEste_ t1 t2

esteTipoDePokemon_SuperaAEste_ :: TipoDePokemon -> TipoDePokemon -> Bool
esteTipoDePokemon_SuperaAEste_ Agua Fuego = True
esteTipoDePokemon_SuperaAEste_ Fuego Planta = True
esteTipoDePokemon_SuperaAEste_ Planta Agua = True
esteTipoDePokemon_SuperaAEste_ _ _ = False

-- PUNTO 2.d:

esMaestroPokemon :: Entrenador -> Bool
esMaestroPokemon e = tienePokemonesDeTipo e Fuego &&  tienePokemonesDeTipo e Agua &&  tienePokemonesDeTipo e Planta

tienePokemonesDeTipo :: Entrenador -> TipoDePokemon -> Bool
tienePokemonesDeTipo e t = algunPokemonEsDeTipo (pokemonesDe e) t

algunPokemonEsDeTipo :: [Pokemon] -> TipoDePokemon -> Bool
algunPokemonEsDeTipo []      _ = False
algunPokemonEsDeTipo (p:pks) t = sonElMismoTipoDePokemon (tipoDePokemon p) t || algunPokemonEsDeTipo pks t

-- PUNTO 3:

data Seniority = Junior | SemiSenior | Senior
    deriving Show

data Proyecto = ConsProyecto String
    deriving Show

proyecto1 :: Proyecto 
proyecto1 = ConsProyecto "proyecto1"

proyecto2 :: Proyecto 
proyecto2 = ConsProyecto "proyecto2"

data Rol = Developer Seniority Proyecto | Management Seniority Proyecto
    deriving Show

rol1 :: Rol
rol1 = Developer Junior proyecto1

rol2 :: Rol
rol2 = Developer Senior proyecto2

rol3 :: Rol
rol3 = Developer Senior proyecto2

data Empresa = ConsEmpresa [Rol]
    deriving Show

empresa1 :: Empresa 
empresa1 = ConsEmpresa [rol1, rol2, rol3]

-- PUNTO 3.a:

proyectos :: Empresa -> [Proyecto]
proyectos (ConsEmpresa rs) = proyectosEnRoles rs

proyectosEnRoles :: [Rol] -> [Proyecto]
proyectosEnRoles []     = []
proyectosEnRoles (r:rs) = if perteneceAProyectos (proyectoDelRol r) (proyectosEnRoles rs)
                            then proyectosEnRoles rs
                            else proyectoDelRol r : proyectosEnRoles rs

proyectoDelRol :: Rol -> Proyecto
proyectoDelRol (Developer _ p)  = p
proyectoDelRol (Management _ p) = p

perteneceAProyectos :: Proyecto -> [Proyecto] -> Bool
perteneceAProyectos pr []     = False
perteneceAProyectos pr (p:ps) = sonElMismoProyecto pr p || perteneceAProyectos pr ps

sonElMismoProyecto :: Proyecto -> Proyecto -> Bool
sonElMismoProyecto (ConsProyecto s1) (ConsProyecto s2) = s1 == s2

-- PUNTO 3.b:

losDevSenior :: Empresa -> [Proyecto] -> Int
--PRECOND: La empresa tiene al menos todos los proyectos de la lista de proyectos.
losDevSenior e p = listaDeDevsSeniorDe_QueEstanEnLosProyectos_ e p

listaDeDevsSeniorDe_QueEstanEnLosProyectos_ :: Empresa -> [Proyecto] -> Int
listaDeDevsSeniorDe_QueEstanEnLosProyectos_ (ConsEmpresa rs) p= listaDeDevsSeniorEn_QueEstanEnLosProyectos_ rs p

listaDeDevsSeniorEn_QueEstanEnLosProyectos_ :: [Rol] -> [Proyecto] -> Int
listaDeDevsSeniorEn_QueEstanEnLosProyectos_ [] p     = 0
listaDeDevsSeniorEn_QueEstanEnLosProyectos_ (x:xs) p = unoSiCeroSino (esSeniorYEstaEnAlgunProyecto_ x p) + listaDeDevsSeniorEn_QueEstanEnLosProyectos_ xs p

esSeniorYEstaEnAlgunProyecto_ :: Rol -> [Proyecto] -> Bool
esSeniorYEstaEnAlgunProyecto_ (Developer sr p) ps  = perteneceAProyectos p ps && esSenior sr
esSeniorYEstaEnAlgunProyecto_ (Management sr p) ps   = perteneceAProyectos p ps && esSenior sr

esSenior :: Seniority -> Bool
esSenior Senior = True
esSenior _      = False

-- PUNTO 3.c:

cantQueTrabajanEn :: [Proyecto] -> Empresa -> Int
cantQueTrabajanEn [] e = 0
cantQueTrabajanEn (p:ps) e = cantEmpleadosQueTrabajanEnElproyecto (empleadosDe e) p + cantQueTrabajanEn ps e

empleadosDe :: Empresa -> [Rol]
empleadosDe (ConsEmpresa rs) = rs

cantEmpleadosQueTrabajanEnElproyecto :: [Rol] -> Proyecto -> Int
cantEmpleadosQueTrabajanEnElproyecto []     _ = 0
cantEmpleadosQueTrabajanEnElproyecto (r:rs) p = unoSiCeroSino ( esteEmpleadoTrabajaEnEsteProyecto r p) + cantEmpleadosQueTrabajanEnElproyecto rs p

esteEmpleadoTrabajaEnEsteProyecto :: Rol -> Proyecto -> Bool
esteEmpleadoTrabajaEnEsteProyecto (Developer _ pe) p = nombreProyecto pe == nombreProyecto p
esteEmpleadoTrabajaEnEsteProyecto (Management _ pe) p = nombreProyecto pe == nombreProyecto p

nombreProyecto :: Proyecto -> String
nombreProyecto (ConsProyecto n) = n

-- PUNTO 3.d:

asignadosPorProyecto :: Empresa -> [(Proyecto, Int)]
asignadosPorProyecto (ConsEmpresa rs) = cantRolesPorProyecto rs

cantRolesPorProyecto :: [Rol] -> [(Proyecto, Int)]
cantRolesPorProyecto []     = []
cantRolesPorProyecto (r:rs) = contarProyectoEn (proyectoDelRol r) (cantRolesPorProyecto rs)

contarProyectoEn :: Proyecto -> [(Proyecto,Int)] -> [(Proyecto,Int)]
contarProyectoEn p []     = [(p,1)]
contarProyectoEn p (y:ys) = if nombreProyecto p == (nombreProyecto (fst y))
                                then (sumarUno y) : ys
                                else y : contarProyectoEn p ys

sumarUno :: (Proyecto, Int) -> (Proyecto, Int) 
sumarUno (p,n) = (p,n+1)
