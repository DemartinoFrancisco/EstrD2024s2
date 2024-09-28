module Set1
(Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList) 
where

data Set a = S [a]     Int
--             Lista   Cantidad de elementos
{- INV.REP.: En S ys n:
        * ys es una lista sin repetidos
        * Si ys es vacio, n es 0
        * Si ys no es vacia, n es la cantidad de elementos de ys
-}

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


emptyS = S [] 0 -- Constante

addS x (S ys n)  = if elem x ys -- Lineal
                        then (S ys n)
                        else (S (x : ys) (n+1))

belongs x (S ys _) = elem x ys -- Lineal

sizeS (S ys n) = n -- Constante

removeS x (S ys n) = if elem x ys -- Cuadratica
                                then (S (removerElementoDeLista x ys) (n-1))
                                else (S ys n)

removerElementoDeLista :: Eq a => a -> [a] -> [a] -- Lineal
-- PRECOND: El elemento se encuentra en la lista
removerElementoDeLista x (y:ys) = if x == y 
                                        then ys
                                        else (y: (removerElementoDeLista x ys))

unionS (S ys _) s = agregarListaASet ys s -- Cuadratica

agregarListaASet :: Eq a => [a] -> Set a -> Set a -- Cuadratica
agregarListaASet []     s = s 
agregarListaASet (x:xs) s = agregarListaASet xs (addS x s)

setToList (S ys n) = ys -- Constante