import Set1 ( Set, addS, belongs, emptyS, setToList, unionS)
--import Set2
import Queue1 ( Queue, emptyQ, isEmptyQ, enqueue, firstQ, dequeue)
-- import Queue2
import Stack1 (Stack, emptySS, isEmptyS, push, top, pop, lenS)
-- import Stack2

-- PUNTO 1 :

-- 1) Constante
-- 2) Lineal
-- 3) Lineal
-- 4) Lineal
-- 5) Cuadratica
-- 6) Lineal
-- 7) Lineal
-- 8) Lineal
-- 9) Cuadratica
-- 10) Lineal
-- 11) Lineal
-- 12) Cuadratica
-- 13) Cuadratica 
-- 14) Lineal
-- 15) Cuadratica

-- PUNTO 2 :

data Tree a = EmptyT | NodeT a (Tree a) (Tree a)
    deriving Show

setPrueba :: Set Int 
setPrueba = addS 3 (addS 2 (addS 1 emptyS))

setPrueba2 :: Set Int
setPrueba2 = addS 3 (addS 8 emptyS)


losQuePertenecen :: Eq a => [a] -> Set a -> [a]
losQuePertenecen []     _   = []
losQuePertenecen (x:xs) set = if belongs x set 
                                then x : (losQuePertenecen xs set) 
                                else losQuePertenecen xs set 

sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos xs = setToList (agregarTodosA xs emptyS)

agregarTodosA :: Eq a => [a] -> Set a -> Set a
agregarTodosA []     s = s
agregarTodosA (x:xs) s = addS x (agregarTodosA xs s)

unirTodos :: Eq a => Tree (Set a) -> Set a
unirTodos EmptyT              = emptyS
unirTodos (NodeT set ts1 ts2) = unionS set (unionS (unirTodos ts1) (unirTodos ts2))

-- PUNTO 3 :

lengthQ :: Queue a -> Int
lengthQ q = if isEmptyQ q
                then 0
                else 1 + lengthQ (dequeue q)

queueToList :: Queue a -> [a]
queueToList q = if isEmptyQ q
                then []
                else (firstQ q) : queueToList (dequeue q)

unionQ :: Queue a -> Queue a -> Queue a
unionQ q1 q2 = if isEmptyQ q2
                then q1
                else enqueue (firstQ q2) (unionQ q1 (dequeue q2))

queue1 :: Queue Int
queue1 = enqueue 3(enqueue 2 (enqueue 1 (emptyQ)))

queue2 :: Queue Int
queue2 = enqueue 6(enqueue 5 (enqueue 4 (emptyQ)))

-- PUNTO 4 :

apilar :: [a] -> Stack a
apilar []     = emptySS
apilar (x:xs) = push x (apilar xs)

desapilar :: Stack a -> [a]
desapilar s = if isEmptyS s 
                then []
                else (top s) : desapilar (pop s)

insertarEnPos :: Int -> a -> Stack a -> Stack a
-- PRECOND: La posicion es valida en el Stack.
insertarEnPos 0 x s = push x s
insertarEnPos n x s = push (top s) (insertarEnPos (n-1) x (pop s))

stack1 :: Stack Int
stack1 = push 3 (push 2 (push 1 (emptySS)))