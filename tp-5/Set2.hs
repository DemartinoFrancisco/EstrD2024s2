module Set2
(Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) 
where

data Set a = S [a]    
    deriving Show
--             Lista


emptyS :: Set a
-- Crea un conjunto vacio.
addS :: Eq a => a -> Set a -> Set a
-- Dados un elemento y un conjunto, agrega el elemento al conjunto
belongs :: Eq a => a -> Set a -> Bool
-- Dados un elemento y un conjunto, agrega el elemento al conjunto
sizeS :: Eq a => Set a -> Int
-- Dado un conjunto, devuelva la cantidad de elementos que posee
removeS :: Eq a => a -> Set a -> Set a
-- Dado un elemento y un conjunto, borra dicho elemento del conjunto
unionS :: Eq a => Set a -> Set a -> Set a
-- Dados dos conjuntos, devuelve un conjunto con todos los elementos de ambos conjuntos.
setToList :: Eq a => Set a -> [a]
-- Dado un conjunto devuelve una lista con todos los elementos distintos del conjunto.


emptyS = S [] -- Constante

addS x (S ys)  = (S (sinRepetidos2(x : ys))) -- Cuadratica

sinRepetidos2 :: Eq a => [a] -> [a] -- Cuadratica
sinRepetidos2 []     = []
sinRepetidos2 (x:xs) = if pertenece x xs
                            then (sinRepetidos2 xs)
                            else x : (sinRepetidos2 xs)

pertenece :: Eq a => a -> [a] -> Bool -- Lineal
pertenece _ [] = False
pertenece t (x:xs) = (t == x) || pertenece t xs


belongs x (S ys) = elem x ys -- Lineal

sizeS (S ys) = cantElementosSinRepetir ys -- Cubica

cantElementosSinRepetir :: Eq a => [a] -> Int -- Cubica
cantElementosSinRepetir xs = longitud (sinRepetidos2 xs)

longitud :: [a] -> Int -- Lineal
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

removeS x (S ys) = if elem x ys -- Cubica
                                then (S (removerElementoDeLista x (sinRepetidos2 ys)) )
                                else (S (sinRepetidos2 ys) )

removerElementoDeLista :: Eq a => a -> [a] -> [a] -- Lineal
-- PRECOND: El elemento se encuentra en la lista
removerElementoDeLista x (y:ys) = if x == y 
                                        then ys
                                        else (y: (removerElementoDeLista x ys))

unionS (S ys) s = agregarListaASet ys s -- Cubica

agregarListaASet :: Eq a => [a] -> Set a -> Set a -- Cubica
agregarListaASet []     s = s 
agregarListaASet (x:xs) s = agregarListaASet xs (addS x s)

setToList (S ys) = sinRepetidos2 ys -- Cuadratica