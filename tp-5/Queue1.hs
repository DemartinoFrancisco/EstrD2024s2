module Queue1 (Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue) where

data Queue a = Q [a] 
            --   Lista
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


emptyQ = (Q [])  -- Constante

isEmptyQ (Q xs) = null xs -- Constante

enqueue x (Q ys) = (Q (ys ++ [x])) -- Constante

firstQ (Q xs) = if null xs -- Constante
            then error "No puede estar vacia la cola, si estas preguntando por su primer elemento"
            else head xs


dequeue (Q xs) = if null xs -- Constante
                then error "No se puede sacar un elemento, si no tiene ningun elemento de por si"
                else (Q (tail xs))

queue1 :: Queue Int
queue1 = (Q [1,2,3])