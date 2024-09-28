module Queue3 (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

data Queue a = Q [a] [a]
            --   FS  BS
    {- INV.REP. : 
            *Si fs se encuentra vacía, entonces la cola se encuentra vacía 
    -}
    deriving Show

emptyQ :: Queue a
--Crea una cola vacía.

isEmptyQ :: Queue a -> Bool
--Dada una cola indica si la cola está vacía.

enqueue :: a -> Queue a -> Queue a
--Dados un elemento y una cola, agrega ese elemento a la cola.

firstQ :: Queue a -> a -- PARCIAL
--Dada una cola devuelve el primer elemento de la cola.

dequeue :: Queue a -> Queue a -- PARCIAL
--Dada una cola la devuelve sin su primer elemento.

emptyQ = (Q [] []) -- Constante

isEmptyQ (Q ys _) = null ys -- Constante

enqueue x (Q xs ys) = if null xs -- Constante
                        then (Q [x] ys) 
                        else (Q xs (ys ++ [x]))

firstQ q = if isEmptyQ q -- Constante
            then error "No puede estar vacia la cola, si estas preguntando por su primer elemento"
            else primerElementoDeQ q

primerElementoDeQ :: Queue a -> a -- Constante
-- PRECOND: La cola debe tener al menos un elemento.
primerElementoDeQ (Q (x:xs) ys) = x

dequeue q = if isEmptyQ q -- Constante
                then error "No se puede sacar un elemento, si no tiene ningun elemento de por si"
                else sacarDeLaQueue q

sacarDeLaQueue :: Queue a -> Queue a -- Constante
-- PRECOND: La cola debe tener al menos un elemento.
sacarDeLaQueue (Q (x:xs) ys) = (Q xs ys)

