module Stack1 (Stack, emptySS, isEmptyS, push, top, pop, lenS) where

data Stack a = S [a]
--               Lista

emptySS :: Stack a
--Crea una pila vacía.

isEmptyS :: Stack a -> Bool
--Dada una pila indica si está vacía.

push :: a -> Stack a -> Stack a
--Dados un elemento y una pila, agrega el elemento a la pila.

top :: Stack a -> a
-- Dada un pila devuelve el elemento del tope de la pila.
-- PRECOND: La pila no debe estar vacia
-- PARCIAL

pop :: Stack a -> Stack a
--Dada una pila devuelve la pila sin el primer elemento.

lenS :: Stack a -> Int
--Dada la cantidad de elementos en la pila.
--Costo: constante.

emptySS = (S [])

isEmptyS (S xs) = null xs

push x (S ys) = (S (x:ys))

top (S xs) = if null xs
                then error "No podes pedir un elemento, si la pila esta vacia"
                else head xs

pop (S xs) = if null xs
                then error "No podes sacar un elemento, si la pila no tiene elementos"
                else (S (tail xs))

lenS (S xs) = length xs