module MapV2 (Map, emptyM, assocM, lookUpM, deleteM, keys) where

data Map k v = M [(k,v)]
    {-  OBSERVACIONES:
                * N hace referencia a la cantidad de pares clave-valor TOTALES.
    -}

emptyM :: Map k v                                                                                        -- O(1)
--Propósito: devuelve un map vacío                                                       
assocM :: Eq k => k -> v -> Map k v -> Map k v                                                           -- O(1)
--Propósito: agrega una asociación clave-valor al map.
lookUpM :: Eq k => k -> Map k v -> Maybe v                                                               -- O(N)
--Propósito: encuentra un valor dado una clave.
deleteM :: Eq k => k -> Map k v -> Map k v                                                               -- O(N)
--Propósito: borra una asociación dada una clave.
keys :: Eq k => Map k v -> [k]                                                                           -- O(N^2)
--Propósito: devuelve las claves del map.

emptyM = M []



assocM k v (M kvs) = (M ((k,v):kvs))



lookUpM k (M kvs) = buscarClave k kvs

buscarClave :: Eq k => k -> [(k,v)] -> Maybe v                                                           -- O(N)
buscarClave k []          = Nothing
buscarClave k ((k',v):kvs) = if k==k' 
                                then Just v
                                else buscarClave k kvs



deleteM k (M kvs) = (M (borrarClave k kvs))

borrarClave :: Eq k => k -> [(k,v)] -> [(k,v)]                                                           -- O(N)
borrarClave k []            = []
borrarClave k ((k',v'):kvs) = if k==k'
                                then kvs
                                else (k',v') : borrarClave k kvs



keys (M kvs) = sinRepetidos (clavesDe kvs)

clavesDe :: [(k,v)] -> [k]                                                                               -- O(N)
clavesDe []          = []
clavesDe ((k,v):kvs) = k : clavesDe kvs

sinRepetidos :: Eq a => [a] -> [a]                                                                       -- O(N^2)
sinRepetidos []     = []
sinRepetidos (x:xs) = if pertenece x xs 
                        then sinRepetidos xs
                        else x : sinRepetidos xs

pertenece :: Eq a => a -> [a] -> Bool                                                                    -- O(N)
pertenece _ []     = False
pertenece t (x:xs) = (t == x) || pertenece t xs