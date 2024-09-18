-- PUNTO 1 :

data Pizza = Prepizza 
            | Capa Ingrediente Pizza
    deriving Show

data Ingrediente = Salsa
                | Queso
                | Jamon
                | Aceitunas Int
    deriving Show

pizza1 :: Pizza
pizza1 = Capa Queso pizza2

pizza2 :: Pizza
pizza2 = Capa Salsa pizza3

pizza3 :: Pizza
pizza3 = Capa Jamon pizza4

pizza4 :: Pizza
pizza4 = Capa (Aceitunas 8) pizza5

pizza5 :: Pizza
pizza5 = Prepizza

pizza6 :: Pizza
pizza6 = Capa Salsa pizza7

pizza7 :: Pizza
pizza7 = Capa Queso Prepizza

listaIngredientes1 :: [Ingrediente]
listaIngredientes1 = [Salsa, Queso, (Aceitunas 9), Salsa, Jamon, Jamon]

listaIngredientes2 :: [Ingrediente]
listaIngredientes2 = []

-- PUNTO 1.a :

cantidadDeCapas :: Pizza -> Int
cantidadDeCapas Prepizza       = 0
cantidadDeCapas (Capa ing piz) = 1 + cantidadDeCapas piz

-- PUNTO 1.b :

armarPizza :: [Ingrediente] -> Pizza
armarPizza []      = Prepizza
armarPizza (i:igs) = (Capa i (armarPizza (igs)))

-- PUNTO 1.c :

sacarJamon :: Pizza -> Pizza
sacarJamon Prepizza     = Prepizza
sacarJamon (Capa ing p) = if esJamon ing 
                            then sacarJamon p 
                            else (Capa ing (sacarJamon p))

esJamon :: Ingrediente -> Bool
esJamon Jamon = True
esJamon _     = False

-- PUNTO 1.d :

tieneSoloSalsaYQueso :: Pizza -> Bool
tieneSoloSalsaYQueso Prepizza     = True
tieneSoloSalsaYQueso (Capa ing p) = esSalsaOQueso ing && tieneSoloSalsaYQueso p

esSalsaOQueso :: Ingrediente -> Bool
esSalsaOQueso Salsa = True
esSalsaOQueso Queso = True
esSalsaOQueso _     = False

-- PUNTO 1.e :

duplicarAceitunas :: Pizza -> Pizza
duplicarAceitunas Prepizza     = Prepizza
duplicarAceitunas (Capa ing p) = if sonAceitunas ing 
                                    then (Capa (dobleDeAceitunas (ing)) (duplicarAceitunas p))
                                    else (Capa ing (duplicarAceitunas p))

dobleDeAceitunas :: Ingrediente -> Ingrediente
-- PRECOND: El ingrediente es una aceituna
dobleDeAceitunas (Aceitunas n) = (Aceitunas (n*2))

sonAceitunas :: Ingrediente -> Bool
sonAceitunas (Aceitunas _) = True
sonAceitunas _            = False

-- PUNTO 1.f :

cantCapasPorPizza :: [Pizza] -> [(Int,Pizza)]
cantCapasPorPizza []     = []
cantCapasPorPizza (p:ps) = (cantidadDeCapas p , p) : cantCapasPorPizza ps


-- PUNTO 2 :

data Dir = Izq | Der
    deriving Show
data Objeto = Tesoro | Chatarra
    deriving Show
data Cofre = Cofre [Objeto]
    deriving Show
data Mapa = Fin Cofre
            | Bifurcacion Cofre Mapa Mapa
    deriving Show

mapa1 :: Mapa 
mapa1 = Bifurcacion cofre4 mapa5 mapa5

mapa2 :: Mapa 
mapa2 = Bifurcacion cofre4 mapa1 mapa4

mapa3 :: Mapa
mapa3 = Bifurcacion cofre4 mapa4 mapa2

mapa4 :: Mapa
mapa4 = Fin cofre2

mapa5 :: Mapa
mapa5 = Fin cofre1

cofre1 :: Cofre
cofre1 = Cofre objetos1

cofre2 :: Cofre 
cofre2 = Cofre objetos2

cofre3 :: Cofre
cofre3 = Cofre objetos3

cofre4 :: Cofre
cofre4 = Cofre objetos4

objetos1 :: [Objeto] 
objetos1 = [Tesoro, Chatarra]

objetos2 :: [Objeto]
objetos2 = [Tesoro, Tesoro]

objetos3 :: [Objeto]
objetos3 = [Chatarra, Chatarra]

objetos4 :: [Objeto]
objetos4 = []

-- PUNTO 2.1 : 

hayTesoro :: Mapa -> Bool
hayTesoro (Fin c)               = hayTesoroEnCofre c
hayTesoro (Bifurcacion c m1 m2) = hayTesoroEnCofre c || hayTesoro m1 || hayTesoro m2

hayTesoroEnCofre :: Cofre -> Bool
hayTesoroEnCofre (Cofre objs) = hayTesoroEnObjs objs

hayTesoroEnObjs :: [Objeto] -> Bool
hayTesoroEnObjs []         = False
hayTesoroEnObjs (obj:objs) = esTesoro obj || hayTesoroEnObjs objs

esTesoro :: Objeto -> Bool
esTesoro Tesoro = True
esTesoro _      = False

-- PUNTO 2.2 :

hayTesoroEn :: [Dir] -> Mapa -> Bool
hayTesoroEn []     m                     = hayTesoroEnMapaAca m
hayTesoroEn (d:ds) (Bifurcacion _ m1 m2) = if esIzq d 
                                            then hayTesoroEn ds m1
                                            else hayTesoroEn ds m2
hayTesoroEn (d:ds) (Fin _)               = False

hayTesoroEnMapaAca :: Mapa -> Bool
hayTesoroEnMapaAca (Bifurcacion c _ _) = hayTesoroEnCofre c
hayTesoroEnMapaAca (Fin c)             = hayTesoroEnCofre c

esIzq :: Dir -> Bool
esIzq Izq = True
esIzq _   = False

-- PUNTO 2.3 :

caminoAlTesoro :: Mapa -> [Dir]
--PRECOND: Existe un tesoro y es unico
caminoAlTesoro (Fin _)               = []
caminoAlTesoro (Bifurcacion c m1 m2) = if hayTesoroEnCofre c 
                                        then []
                                        else if hayTesoro m1
                                                then Izq :caminoAlTesoro m1
                                                else Der :caminoAlTesoro m2

-- PUNTO 2.4 :

caminoDeLaRamaMasLarga :: Mapa -> [Dir]
caminoDeLaRamaMasLarga (Fin _)               = []
caminoDeLaRamaMasLarga (Bifurcacion _ m1 m2) = if length(caminoDeLaRamaMasLarga m1) >= length(caminoDeLaRamaMasLarga m2)
                                                    then Izq : caminoDeLaRamaMasLarga m1
                                                    else Der : caminoDeLaRamaMasLarga m2

-- PUNTO 2.5 :

tesorosPorNivel :: Mapa -> [[Objeto]]
tesorosPorNivel (Fin c)               = [listaConTesorosEnCofre c]
tesorosPorNivel (Bifurcacion c m1 m2) = listaConTesorosEnCofre c : (unirListasElementoAElemento (tesorosPorNivel m1) (tesorosPorNivel m2))

listaConTesorosEnCofre :: Cofre -> [Objeto]
listaConTesorosEnCofre (Cofre objs) = listaConTesorosEntreObjs objs

listaConTesorosEntreObjs :: [Objeto] -> [Objeto]
listaConTesorosEntreObjs []         = []
listaConTesorosEntreObjs (obj:objs) = singularSi obj (esTesoro obj) ++ listaConTesorosEntreObjs objs

singularSi :: a -> Bool -> [a]
singularSi x True  = [x]
singularSi x False = []

unirListasElementoAElemento :: [[a]] -> [[a]] -> [[a]]
unirListasElementoAElemento []     ys     = ys
unirListasElementoAElemento xs     []     = xs
unirListasElementoAElemento (x:xs) (y:ys) = (x ++ y) : unirListasElementoAElemento xs ys

-- PUNTO 2.6 :

todosLosCaminos :: Mapa -> [[Dir]]
todosLosCaminos (Fin _)               = [[]] 
todosLosCaminos (Bifurcacion _ m1 m2) = (consACada Izq (todosLosCaminos m1)) ++ (consACada Der (todosLosCaminos m2))

consACada :: a -> [[a]] -> [[a]]
consACada x []       = []
consACada x (xs:xss) = (x:xs) : consACada x xss

-- PUNTO 3 :

data Componente = LanzaTorpedos | Motor Int | Almacen [Barril]
    deriving Show
data Barril = Comida | Oxigeno | Torpedo | Combustible
    deriving Show
data Sector = S SectorId [Componente] [Tripulante]
    deriving Show
type SectorId = String
type Tripulante = String
data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show
data Nave = N (Tree Sector)
    deriving Show

nave1 :: Nave 
nave1 = N (treeSector1)

treeSector1 :: Tree Sector 
treeSector1 = NodeT sector1 (treeSectorVacio) (treeSector2)

treeSectorVacio :: Tree Sector
treeSectorVacio = EmptyT

sector1 :: Sector 
sector1 = (S "40" componentes1 tripulantes1)

componentes1 :: [Componente] 
componentes1 = [LanzaTorpedos, Motor 7, Almacen barriles1]

barriles1 :: [Barril]
barriles1 = [Comida,Oxigeno,Torpedo,Combustible]

tripulantes1 :: [Tripulante]
tripulantes1 = ["Ernesto","Raul"]

treeSector2 :: Tree Sector 
treeSector2 = NodeT sector2 (treeSectorVacio) (treeSectorVacio)

sector2 :: Sector 
sector2 = (S "3" componentes2 tripulantes2)

componentes2 :: [Componente]
componentes2 = [LanzaTorpedos, Motor 3, Almacen barriles2]

barriles2 :: [Barril]
barriles2 = [Comida,Comida,Comida]

tripulantes2 :: [Tripulante]
tripulantes2 = ["Jose","Maria","Raul","Francisco","Francisco"]

-- PUNTO 3.1 :

sectores :: Nave -> [SectorId]
sectores (N ts) = sectoresDeArbolDeSectores ts

sectoresDeArbolDeSectores :: Tree Sector -> [SectorId]
sectoresDeArbolDeSectores EmptyT          = []
sectoresDeArbolDeSectores (NodeT s t1 t2) = idDeSector s : (sectoresDeArbolDeSectores t1 ++ sectoresDeArbolDeSectores t2)

idDeSector :: Sector -> SectorId
idDeSector (S si _ _) = si

-- PUNTO 3.2:

poderDePropulsion :: Nave -> Int
poderDePropulsion (N ts) = poderDePropulsionDeArbolDeSectores ts

poderDePropulsionDeArbolDeSectores :: Tree Sector -> Int
poderDePropulsionDeArbolDeSectores EmptyT          = 0
poderDePropulsionDeArbolDeSectores (NodeT s t1 t2) = (poderDePropulsionDeSector s) + poderDePropulsionDeArbolDeSectores t1 + poderDePropulsionDeArbolDeSectores t2

poderDePropulsionDeSector :: Sector -> Int
poderDePropulsionDeSector (S _ c _) = poderDePropulsionDeComponentes c

poderDePropulsionDeComponentes :: [Componente] -> Int
poderDePropulsionDeComponentes []     = 0
poderDePropulsionDeComponentes (c:cs) = poderDePropulsionDeComponente c + poderDePropulsionDeComponentes cs

poderDePropulsionDeComponente :: Componente -> Int
poderDePropulsionDeComponente (Motor n) = n
poderDePropulsionDeComponente _         = 0

-- PUNTO 3.3 :

barriles :: Nave -> [Barril]
barriles (N ts) = barrilesDeArbolDeSectores ts

barrilesDeArbolDeSectores :: Tree Sector -> [Barril]
barrilesDeArbolDeSectores EmptyT          = []
barrilesDeArbolDeSectores (NodeT s t1 t2) = (barrilesDeSector s) ++ barrilesDeArbolDeSectores t1 ++ barrilesDeArbolDeSectores t2

barrilesDeSector :: Sector -> [Barril]
barrilesDeSector (S _ c _) = barrilesEnComponentes c 

barrilesEnComponentes :: [Componente] -> [Barril]
barrilesEnComponentes []     = []
barrilesEnComponentes (c:cs) = barrilesEnComponente c ++ barrilesEnComponentes cs

barrilesEnComponente :: Componente -> [Barril]
barrilesEnComponente (Almacen bs) = bs
barrilesEnComponente _            = []

-- PUNTO 3.4 :

agregarASector :: [Componente] -> SectorId -> Nave -> Nave
agregarASector [] si n      = n
agregarASector cs si (N ts) = (N (agregarASectorDeArbolDeSectoresSiExiste cs si ts) )

agregarASectorDeArbolDeSectoresSiExiste :: [Componente] -> SectorId -> Tree Sector -> Tree Sector
agregarASectorDeArbolDeSectoresSiExiste cs si EmptyT          = EmptyT
agregarASectorDeArbolDeSectoresSiExiste cs si (NodeT s t1 t2) = if elSectorConIdEsEste s si
                                                                    then (NodeT (sectorConComponentesActualizados s cs) t1 t2)     
                                                                    else (NodeT s (agregarASectorDeArbolDeSectoresSiExiste cs si t1) (agregarASectorDeArbolDeSectoresSiExiste cs si t2))

elSectorConIdEsEste :: Sector -> SectorId -> Bool
elSectorConIdEsEste (S id _ _) si = (id == si)

sectorConComponentesActualizados :: Sector -> [Componente] -> Sector
sectorConComponentesActualizados (S id cs1 tr) cs2 = (S id (cs1 ++ cs2) tr)

-- PUNTO 3.5 :

asignarTripulanteA :: Tripulante -> [SectorId] -> Nave -> Nave
-- PRECOND: Todos los id de la lista existen en la nave.
asignarTripulanteA tr [] n       = n
asignarTripulanteA tr ids (N ts) = (N (asignarTripulanteAArbolDeSectores tr ids ts))

asignarTripulanteAArbolDeSectores :: Tripulante -> [SectorId] -> Tree Sector -> Tree Sector
-- PRECOND: Todos los id de la lista existen en la nave.
asignarTripulanteAArbolDeSectores tr ids EmptyT          = EmptyT
asignarTripulanteAArbolDeSectores tr ids (NodeT s t1 t2) = if algunIdCoincideConElDeEsteSector s ids
                                                                then (NodeT (sectorConTripulantesActualizados s tr) (asignarTripulanteAArbolDeSectores tr ids t1) (asignarTripulanteAArbolDeSectores tr ids t2))
                                                                else (NodeT s (asignarTripulanteAArbolDeSectores tr ids t1) (asignarTripulanteAArbolDeSectores tr ids t2))

algunIdCoincideConElDeEsteSector :: Sector -> [SectorId] -> Bool
algunIdCoincideConElDeEsteSector (S id _ _) ids = pertenece id ids

pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece t (x:xs) = (t == x) || pertenece t xs

sectorConTripulantesActualizados :: Sector -> Tripulante -> Sector
sectorConTripulantesActualizados (S id cs trs) tr = (S id cs (tr:trs))

-- PUNTO 3.6 :

sectoresAsignados :: Tripulante -> Nave -> [SectorId] 
sectoresAsignados tr (N ts) = sectoresAsignadosEnArbolDeSectores tr ts

sectoresAsignadosEnArbolDeSectores :: Tripulante -> Tree Sector -> [SectorId]
sectoresAsignadosEnArbolDeSectores tr EmptyT          = []
sectoresAsignadosEnArbolDeSectores tr (NodeT s t1 t2) = (idDeSectorSiTieneTripulante tr s) ++ sectoresAsignadosEnArbolDeSectores tr t1 ++ sectoresAsignadosEnArbolDeSectores tr t2

idDeSectorSiTieneTripulante :: Tripulante -> Sector -> [SectorId]
idDeSectorSiTieneTripulante tr (S id _ trs) = if pertenece tr trs
                                                then [id]
                                                else []

-- PUNTO 3.7 :

tripulantes :: Nave -> [Tripulante]
tripulantes (N ts) = tripulantesDeArbolDeSectores ts

tripulantesDeArbolDeSectores :: Tree Sector -> [Tripulante]
tripulantesDeArbolDeSectores EmptyT          = []
tripulantesDeArbolDeSectores (NodeT s t1 t2) = sinRepetidos (tripulantesDeSector s ++ tripulantesDeArbolDeSectores t1 ++ tripulantesDeArbolDeSectores t2)

tripulantesDeSector :: Sector -> [Tripulante]
tripulantesDeSector (S _ _ trs) = trs

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos []     = []
sinRepetidos (x:xs) = if pertenece x xs
                        then sinRepetidos xs
                        else x : sinRepetidos xs

-- PUNTO 4 :

type Presa = String -- nombre de presa
type Territorio = String -- nombre de territorio
type Nombre = String -- nombre de lobo
data Lobo = Cazador Nombre [Presa] Lobo Lobo Lobo
            | Explorador Nombre [Territorio] Lobo Lobo
            | Cria Nombre
    deriving Show
data Manada = M Lobo
    deriving Show

-- PUNTO 4.1 :

manada1 :: Manada
manada1 = M lobo1

lobo1 :: Lobo
lobo1 = (Cazador "Jaime" presas1 lobo5 lobo3 lobo4)

presas1 :: [Presa] 
presas1 = ["Bufalo","Mono","Serpiente","Vibora","Tutancamon","PieGrande","Goku"]

lobo2 :: Lobo
lobo2 = (Explorador "Francisco" territorios1 lobo3 lobo4)

territorios1 :: [Territorio]
territorios1 = []

lobo3 :: Lobo
lobo3 = (Explorador "Enrique" territorios1 lobo2 lobo5)

territorios2 :: [Territorio]
territorios2 = ["Nebraska","Pekin"]

lobo4 :: Lobo
lobo4 = (Cria "Robert")

lobo5 :: Lobo
lobo5 = (Cria "Alfred")

lobo6 :: Lobo
lobo6 = (Cazador "Alfredo" presas1 lobo4 lobo4 lobo4)

-- PUNTO 4.2 :

buenaCaza :: Manada -> Bool
buenaCaza (M l) = cantidadDeAlimentoCazado l > cantidadDeCrias l

cantidadDeAlimentoCazado :: Lobo -> Int
cantidadDeAlimentoCazado (Cazador _ pr l1 l2 l3) = length pr + cantidadDeAlimentoCazado l1 + cantidadDeAlimentoCazado l2 + cantidadDeAlimentoCazado l3
cantidadDeAlimentoCazado (Explorador _ _ l1 l2)  = cantidadDeAlimentoCazado l1 + cantidadDeAlimentoCazado l2
cantidadDeAlimentoCazado (Cria _)                = 0

cantidadDeCrias :: Lobo -> Int
cantidadDeCrias (Cazador _ _ l1 l2 l3) = cantidadDeCrias l1 + cantidadDeCrias l2 + cantidadDeCrias l3
cantidadDeCrias (Explorador _ _ l1 l2) = cantidadDeCrias l1 + cantidadDeCrias l2
cantidadDeCrias (Cria _)               = 1

-- PUNTO 4.3 :

elAlfa :: Manada -> (Nombre, Int)
elAlfa (M l) = elAlfaDe l

elAlfaDe :: Lobo -> (Nombre, Int)
elAlfaDe (Cazador n prs l1 l2 l3) = elQueMasCazoDe [(n,length prs) ,elAlfaDe l1, elAlfaDe l2, elAlfaDe l3]
elAlfaDe (Explorador n trs l1 l2) = elQueMasCazoDe [elAlfaDe l1, elAlfaDe l2]
elAlfaDe (Cria n)                 = (n,0)

elQueMasCazoDe :: [(Nombre,Int)] -> (Nombre,Int)
-- PRECOND: La lista no puede ser vacia
elQueMasCazoDe (nc:[])  = nc
elQueMasCazoDe (nc:ncs) = elegirElQueMasCazoEntre nc (elQueMasCazoDe ncs)

elegirElQueMasCazoEntre :: (Nombre,Int) -> (Nombre,Int) -> (Nombre,Int)
elegirElQueMasCazoEntre (n1,c1) (n2,c2) = if c1 >= c2
                                            then (n1,c1)
                                            else (n2,c2)

-- PUNTO 4.4 :

losQueExploraron :: Territorio -> Manada -> [Nombre]
losQueExploraron tr (M l) = losQueExploraronDeLobo tr l

losQueExploraronDeLobo :: Territorio -> Lobo -> [Nombre]
losQueExploraronDeLobo tr (Cazador _ _ l1 l2 l3)   = sinRepetidos (losQueExploraronDeLobo tr l1 ++ losQueExploraronDeLobo tr l2)
losQueExploraronDeLobo tr (Explorador n trs l1 l2) = sinRepetidos ((nombreDeLoboEnListaSiExploroEsteTerritorio tr n trs) ++ losQueExploraronDeLobo tr l1 ++ losQueExploraronDeLobo tr l2)
losQueExploraronDeLobo tr (Cria _)                 = []

nombreDeLoboEnListaSiExploroEsteTerritorio :: Territorio -> Nombre -> [Territorio] -> [Nombre]
nombreDeLoboEnListaSiExploroEsteTerritorio tr n trs = if pertenece tr trs 
                                                then [n]
                                                else []

-- PUNTO 4.5 :

exploradoresPorTerritorio :: Manada -> [(Territorio,[Nombre])]
exploradoresPorTerritorio (M l) = exploradoresPorTerritorioDeLobo l

exploradoresPorTerritorioDeLobo :: Lobo -> [(Territorio,[Nombre])]
exploradoresPorTerritorioDeLobo (Cazador _ _ l1 l2 l3)   = sinRepetidos (exploradoresPorTerritorioDeLobo l1 ++ exploradoresPorTerritorioDeLobo l2 ++ exploradoresPorTerritorioDeLobo l3)
exploradoresPorTerritorioDeLobo (Explorador n trs l1 l2) = siAlgunTerritorioYaEstaLoAgregoSinoCreoUnNuevoPar n trs (sinRepetidos (exploradoresPorTerritorioDeLobo l1 ++ exploradoresPorTerritorioDeLobo l2))
exploradoresPorTerritorioDeLobo (Cria _)                 = []

siAlgunTerritorioYaEstaLoAgregoSinoCreoUnNuevoPar :: Nombre -> [Territorio] -> [(Territorio,[Nombre])] -> [(Territorio,[Nombre])]
siAlgunTerritorioYaEstaLoAgregoSinoCreoUnNuevoPar n []     tns = tns
siAlgunTerritorioYaEstaLoAgregoSinoCreoUnNuevoPar n (t:ts) []  = [(t,[n])] ++ siAlgunTerritorioYaEstaLoAgregoSinoCreoUnNuevoPar n ts []
siAlgunTerritorioYaEstaLoAgregoSinoCreoUnNuevoPar n (t:ts) tns = siEncontrasElTerritorioEntreLosParesAgregaloSinoCreaUnParNuevo n t tns ++ siAlgunTerritorioYaEstaLoAgregoSinoCreoUnNuevoPar n ts tns

siEncontrasElTerritorioEntreLosParesAgregaloSinoCreaUnParNuevo :: Nombre -> Territorio -> [(Territorio,[Nombre])] -> [(Territorio,[Nombre])]
siEncontrasElTerritorioEntreLosParesAgregaloSinoCreaUnParNuevo n t []          = [(t,[n])]
siEncontrasElTerritorioEntreLosParesAgregaloSinoCreaUnParNuevo n t (tns:tsnss) = if t == (territorioDelPar tns)
                                                                                    then (agregarNombreAPar n tns) : tsnss
                                                                                    else siEncontrasElTerritorioEntreLosParesAgregaloSinoCreaUnParNuevo n t tsnss

territorioDelPar :: (Territorio,[Nombre]) -> Territorio
territorioDelPar (t,ns) = t

agregarNombreAPar :: Nombre -> (Territorio,[Nombre]) -> (Territorio,[Nombre])
agregarNombreAPar n (t,ns) = (t,(n:ns))

-- PUNTO 4.6 :

cazadoresSuperioresDe :: Nombre -> Manada -> [Nombre]
-- PRECOND: Hay un lobo con dicho nombre en la manada y es único.
cazadoresSuperioresDe n (M l) = cazadoresSuperioresDeDeLobo n l

cazadoresSuperioresDeDeLobo :: Nombre -> Lobo -> [Nombre]
cazadoresSuperioresDeDeLobo n1 (Cazador n2 _ l1 l2 l3) = if n1 == n2
                                                            then []
                                                            else if esteLoboOSusSubordinadosTieneNombre n1 l1
                                                                then n2 : (cazadoresSuperioresDeDeLobo n1 l1)
                                                                else if esteLoboOSusSubordinadosTieneNombre n1 l2
                                                                    then n2 : (cazadoresSuperioresDeDeLobo n1 l2)
                                                                    else n2 : (cazadoresSuperioresDeDeLobo n1 l3)
cazadoresSuperioresDeDeLobo n1 (Explorador n2 _ l1 l2) = if n1 == n2
                                                            then []
                                                            else if esteLoboOSusSubordinadosTieneNombre n1 l1
                                                                then (cazadoresSuperioresDeDeLobo n1 l1)
                                                                else (cazadoresSuperioresDeDeLobo n1 l2)
cazadoresSuperioresDeDeLobo n1 (Cria n2)               = []

esteLoboOSusSubordinadosTieneNombre :: Nombre -> Lobo -> Bool
esteLoboOSusSubordinadosTieneNombre n1 (Cazador n2 _ l1 l2 l3) = (n1 == n2) || esteLoboOSusSubordinadosTieneNombre n1 l1 || esteLoboOSusSubordinadosTieneNombre n1 l2 || esteLoboOSusSubordinadosTieneNombre n1 l3
esteLoboOSusSubordinadosTieneNombre n1 (Explorador n2 _ l1 l2) = (n1 == n2) || esteLoboOSusSubordinadosTieneNombre n1 l1 || esteLoboOSusSubordinadosTieneNombre n1 l2
esteLoboOSusSubordinadosTieneNombre n1 (Cria n)                = (n1 == n)
