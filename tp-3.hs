
-- PUNTO 1.1:
data Color = Azul | Rojo
    deriving Show
data Celda = Bolita Color Celda | CeldaVacia
    deriving Show

celda1 :: Celda
celda1 = Bolita Rojo (Bolita Rojo (Bolita Azul CeldaVacia))

celda2 :: Celda
celda2 = Bolita Rojo CeldaVacia 

celda3 :: Celda
celda3 = CeldaVacia

-- PUNTO 1.1.a:

nroBolitas :: Color -> Celda -> Int
nroBolitas c CeldaVacia     = 0
nroBolitas c (Bolita co ce) = unoSiCeroSino (sonDelMismoColor c co) + nroBolitas c ce

unoSiCeroSino :: Bool -> Int
unoSiCeroSino True = 1
unoSiCeroSino _ = 0

sonDelMismoColor :: Color -> Color -> Bool
sonDelMismoColor Azul Azul = True
sonDelMismoColor Rojo Rojo = True
sonDelMismoColor _    _    = False

-- PUNTO 1.1.b:

poner :: Color -> Celda -> Celda
poner c celda = Bolita c celda

-- PUNTO 1.1.c:

sacar :: Color -> Celda -> Celda
sacar c CeldaVacia     = CeldaVacia 
sacar c celda          = siHayUnaBolitaColor_Sacarla c celda

siHayUnaBolitaColor_Sacarla :: Color -> Celda -> Celda
siHayUnaBolitaColor_Sacarla c celda = if (nroBolitas c celda > 0)
                                        then sacarBolitaColor c celda
                                        else celda
    
sacarBolitaColor :: Color -> Celda -> Celda
sacarBolitaColor c (Bolita co ce) =  if sonDelMismoColor c co
                                        then ce
                                        else sacarBolitaColor c ce

-- PUNTO 1.1.d:

ponerN :: Int -> Color -> Celda -> Celda
ponerN 0 c celda  = celda
ponerN n c celda  = Bolita c (ponerN (n-1) c celda) 



-- PUNTO 1.2:

data Objeto = Cacharro | Tesoro
data Camino = Fin | Cofre [Objeto] Camino | Nada Camino

camino1 :: Camino 
camino1 = Cofre objetos1 camino10

camino10 :: Camino
camino10 = Cofre objetos1 Fin

camino2 :: Camino
camino2 = Nada camino3

camino3 :: Camino 
camino3 = Fin

camino4 :: Camino
camino4 = Cofre [Cacharro, Tesoro] camino1

objetos1 :: [Objeto] 
objetos1 = [Cacharro, Cacharro, Tesoro, Tesoro]

-- PUNTO 1.2.a:

hayTesoro :: Camino -> Bool
hayTesoro Fin             = False
hayTesoro (Cofre objs ca) = algunObjetoEsTesoro objs || hayTesoro ca
hayTesoro (Nada ca)       = hayTesoro ca  

algunObjetoEsTesoro :: [Objeto] -> Bool
algunObjetoEsTesoro []           = False
algunObjetoEsTesoro (obj : objs) = esteObjetoEsTesoro obj || algunObjetoEsTesoro objs

esteObjetoEsTesoro :: Objeto -> Bool
esteObjetoEsTesoro Tesoro = True 
esteObjetoEsTesoro _      = False

-- PUNTO 1.2.b:

pasosHastaTesoro :: Camino -> Int
-- PRECOND: Tiene que haber al menos 1 tesoro
pasosHastaTesoro (Cofre objs ca) = sialgunObjetoEsTesoroDevuelvoCeroSinoSigoBuscando objs ca
pasosHastaTesoro (Nada ca)       = 1 + pasosHastaTesoro ca

sialgunObjetoEsTesoroDevuelvoCeroSinoSigoBuscando :: [Objeto] -> Camino -> Int
sialgunObjetoEsTesoroDevuelvoCeroSinoSigoBuscando objs ca = if algunObjetoEsTesoro objs 
                                                                then 0
                                                                else 1 + pasosHastaTesoro ca

-- PUNTO 1.2.c:

hayTesoroEn :: Int -> Camino -> Bool
hayTesoroEn 0 ce              = algunObjetoAcaEsTesoro ce
hayTesoroEn n (Nada ca)       = hayTesoroEn (n-1) ca
hayTesoroEn n (Cofre objs ca) = hayTesoroEn (n-1) ca
hayTesoroEn n Fin             = False

algunObjetoAcaEsTesoro :: Camino -> Bool
algunObjetoAcaEsTesoro (Cofre objs ca) = algunObjetoEsTesoro objs
algunObjetoAcaEsTesoro _               = False

-- PUNTO 1.3.d:

alMenosNTesoros :: Int -> Camino -> Bool
alMenosNTesoros n ca = n <= cantidadDeTesorosEnCamino ca

cantidadDeTesorosEnCamino :: Camino -> Int
cantidadDeTesorosEnCamino Fin             = 0
cantidadDeTesorosEnCamino (Nada ca)       = cantidadDeTesorosEnCamino ca
cantidadDeTesorosEnCamino (Cofre objs ca) = cantidadDeTesorosAca objs + cantidadDeTesorosEnCamino ca

cantidadDeTesorosAca :: [Objeto] -> Int
cantidadDeTesorosAca []         = 0
cantidadDeTesorosAca (obj:objs) = unoSiCeroSino (esteObjetoEsTesoro obj) + cantidadDeTesorosAca objs

-- PUNTO 1.3.e:

cantTesorosEntre :: Int -> Int -> Camino -> Int
--PRECOND: m es mayor que n
cantTesorosEntre 0 m ca              = cantidadDeTesorosEnmPasos m ca
cantTesorosEntre 1 m ca              = cantidadDeTesorosEnmPasos m ca
cantTesorosEntre n m Fin             = 0
cantTesorosEntre n m (Cofre objs ca) = cantTesorosEntre (n-1) (m-1) ca
cantTesorosEntre n m (Nada ca)       = cantTesorosEntre (n-1) (m-1) ca

cantidadDeTesorosEnmPasos :: Int -> Camino -> Int
cantidadDeTesorosEnmPasos 0 _               = 0
cantidadDeTesorosEnmPasos n Fin             = 0
cantidadDeTesorosEnmPasos n (Cofre objs ca) = cantidadDeTesorosAca objs + cantidadDeTesorosEnmPasos (n-1) ca
cantidadDeTesorosEnmPasos n (Nada ca)       = cantidadDeTesorosEnmPasos (n-1) ca

-- PUNTO 2.1:

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

treeNumero1 :: Tree Int
treeNumero1 = NodeT 4 treeNumero2 treeNumero3

treeNumero2 :: Tree Int
treeNumero2 = NodeT 7 treeNumero3 treeNumero4

treeNumero3 :: Tree Int
treeNumero3 = NodeT 4 treeNumero4 EmptyT

treeNumero4 :: Tree Int
treeNumero4 = NodeT 5 EmptyT EmptyT

treeVacio :: Tree a
treeVacio = EmptyT

-- PUNTO 2.1.1:

sumarT :: Tree Int -> Int
sumarT EmptyT          = 0
sumarT (NodeT n t1 t2) = n + sumarT t1 + sumarT t2

-- PUNTO 2.1.2:

sizeT :: Tree a -> Int
sizeT EmptyT          = 0
sizeT (NodeT a t1 t2) = 1 + sizeT t1 + sizeT t2

-- PUNTO 2.1.3:

mapDobleT :: Tree Int -> Tree Int
mapDobleT EmptyT          = EmptyT
mapDobleT (NodeT n t1 t2) = NodeT (n*2) (mapDobleT t1) (mapDobleT t2)

-- PUNTO 2.1.4:

perteneceT :: Eq a => a -> Tree a -> Bool
perteneceT _ EmptyT          = False
perteneceT j (NodeT x t1 t2) = j == x || perteneceT j t1 || perteneceT j t2

-- PUNTO 2.1.5:

aparicionesT :: Eq a => a -> Tree a -> Int
aparicionesT j EmptyT          = 0
aparicionesT j (NodeT x t1 t2) = unoSiCeroSino (j == x) + aparicionesT j t1 + aparicionesT j t2 

-- PUNTO 2.1.6:

leaves :: Tree a -> [a]
leaves EmptyT                  = []
leaves (NodeT x EmptyT EmptyT) = [x]
leaves (NodeT _ t1 t2)         = leaves t1 ++ leaves t2

-- PUNTO 2.1.7:

heightT :: Tree a -> Int
heightT EmptyT          = 0
heightT (NodeT x t1 t2) = 1 + heightT (ramaMasLargaEntre t1 t2)

ramaMasLargaEntre :: Tree a -> Tree a -> Tree a
ramaMasLargaEntre r1 r2 = if (heightT r1 > heightT r2)
                                then r1
                                else r2

-- PUNTO 2.1.8:

mirrorT :: Tree a -> Tree a 
mirrorT EmptyT          = EmptyT
mirrorT (NodeT x t1 t2) = NodeT x (mirrorT t2) (mirrorT t1)

-- PUNTO 2.1.9:

toList :: Tree a -> [a]
toList EmptyT          = []
toList (NodeT x t1 t2) = toList t1 ++ [x] ++ toList t2 

-- PUNTO 2.1.10:

levelN :: Int -> Tree a -> [a]
levelN _ EmptyT          = []
levelN 0 (NodeT x t1 t2) = [x]
levelN n (NodeT x t1 t2) = levelN (n-1) t1 ++ levelN (n-1) t2

-- PUNTO 2.1.11:

listPerLevel :: Tree a -> [[a]]
listPerLevel EmptyT          = []
listPerLevel (NodeT x t1 t2) = [[x]] ++ agruparPorNivel (listPerLevel t1) (listPerLevel t2)

agruparPorNivel :: [[a]] -> [[a]] -> [[a]]
agruparPorNivel []     []     = []
agruparPorNivel xs     []     = xs
agruparPorNivel []     ys     = ys
agruparPorNivel (x:xs) (y:ys) = [x ++ y] ++ agruparPorNivel xs ys

-- PUNTO 2.1.12:

ramaMasLarga :: Tree a -> [a]
ramaMasLarga EmptyT          = []
ramaMasLarga (NodeT x r1 r2) = [x] ++ laListaMasLargaEntre (ramaMasLarga r1) (ramaMasLarga r2)

laListaMasLargaEntre :: [a] -> [a] -> [a]
laListaMasLargaEntre xs ys = if length xs > length ys
                                then xs
                                else ys

-- PUNTO 2.1.13:

todosLosCaminos :: Tree a -> [[a]]
todosLosCaminos EmptyT          = []
todosLosCaminos (NodeT x r1 r2) = [x] : consACada x (todosLosCaminos r1) ++ (consACada x (todosLosCaminos r2))

consACada :: a -> [[a]] -> [[a]]
consACada x []       = []
consACada x (ys:yss) = (x:ys) : consACada x yss

-- PUNTO 2.2:

data ExpA = Valor Int
            | Sum ExpA ExpA
            | Prod ExpA ExpA
            | Neg ExpA
    deriving Show

exp1 :: ExpA
exp1 = Valor 4

exp2 :: ExpA
exp2 = Valor (-3)

exp3 :: ExpA
exp3 = Sum exp1 exp6

exp4 :: ExpA
exp4 = Prod exp1 exp7

exp5 :: ExpA 
exp5 = Neg exp1

exp6 :: ExpA
exp6 = Valor 0

exp7 :: ExpA
exp7 = Valor 1

exp8 :: ExpA
exp8 = Neg exp2

-- PUNTO 2.2.1:

eval :: ExpA -> Int
eval (Valor n)      = n
eval (Sum ex1 ex2)  = eval (ex1) + eval (ex2)
eval (Prod ex1 ex2) = eval(ex1) * eval(ex2)
eval (Neg ex)       = -eval(ex)

-- PUNTO 2.2.2:

simplificar :: ExpA -> ExpA
simplificar (Valor n)      = (Valor n)
simplificar (Sum ex1 ex2)  = siAlgunaExpresionEsCeroLoSimplifico ex1 ex2
simplificar (Prod ex1 ex2) = simplificoLaMultiplicacionSiCorresponde ex1 ex2
simplificar (Neg ex)       = siYaEsNegativoLoSimplifico ex

siYaEsNegativoLoSimplifico :: ExpA -> ExpA
siYaEsNegativoLoSimplifico e1 = if eval e1 < 0
                                then (Valor (-(eval e1)))
                                else (Valor (eval e1))

siAlgunaExpresionEsCeroLoSimplifico :: ExpA -> ExpA -> ExpA
siAlgunaExpresionEsCeroLoSimplifico e1 e2  = if (eval (e1) == 0 || eval (e2) == 0)
                                                then (Valor 0)
                                                else (Sum e1 e2)

simplificoLaMultiplicacionSiCorresponde :: ExpA -> ExpA -> ExpA
simplificoLaMultiplicacionSiCorresponde e1 e2 = if (eval (e1) == 0 || eval (e2) == 0)
                                                    then (Valor 0)
                                                    else if eval (e1) == 1
                                                            then e1
                                                            else if eval (e2) == 1
                                                                then e2
                                                                else (Prod e1 e2)
