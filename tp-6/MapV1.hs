module MapV1 (Map, emptyM, assocM, lookUpM, deleteM, keys) where

data Map k v = M [(k,v)]
    {-  INV.REP.:
                *En M kvs: 
                    -No puede haber claves repetidas en kvs.
        OBSERVACIONES:
                * n hace referencia a la cantidad de pares clave-valor DISTINTAS.
    -}

emptyM :: Map k v                                                                                        -- O(1)
--Propósito: devuelve un map vacío                                                       
assocM :: Eq k => k -> v -> Map k v -> Map k v                                                           -- O(n)
--Propósito: agrega una asociación clave-valor al map.
--Observacion: Si la clave ya existe en el Map, la sobreescribe, sino crea un nuevo par clave-valor.
lookUpM :: Eq k => k -> Map k v -> Maybe v                                                               -- O(n)
--Propósito: encuentra un valor dado una clave.
deleteM :: Eq k => k -> Map k v -> Map k v                                                               -- O(n)
--Propósito: borra una asociación dada una clave.
keys :: Map k v -> [k]                                                                                   -- O(n)
--Propósito: devuelve las claves del map.

emptyM = (M [])



assocM k v (M kvs) = (M (agregarClave k v kvs))

agregarClave :: Eq k => k -> v -> [(k,v)] -> [(k,v)]                                                     -- O(n)
agregarClave k v []            = [(k,v)]
agregarClave k v ((k',v'):kvs) = if k==k' 
                                    then (k',v)   : kvs
                                    else (k',v') : agregarClave k v kvs



lookUpM k (M kvs) = buscarClave k kvs

buscarClave :: Eq k => k -> [(k,v)] -> Maybe v                                                           -- O(n)
buscarClave k []          = Nothing
buscarClave k ((k',v):kvs) = if k==k' 
                                then Just v
                                else buscarClave k kvs



deleteM k (M kvs) = (M (borrarClave k kvs))

borrarClave :: Eq k => k -> [(k,v)] -> [(k,v)]                                                           -- O(n)
borrarClave k []            = []
borrarClave k ((k',v'):kvs) = if k==k'
                                then kvs
                                else (k',v') : borrarClave k kvs



keys (M kvs) = clavesDe kvs

clavesDe :: [(k,v)] -> [k]                                                                               -- O(n)
clavesDe []          = []
clavesDe ((k,v):kvs) = k : clavesDe kvs