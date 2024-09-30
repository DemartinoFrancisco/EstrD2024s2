module MapV1 (Map, emptyM, keys) where

data Map k v = M [k] [v]
    {-  INV.REP.:
                *En M ks vs: 
                    -La clave ubicada en la posición i está asociada al valor en la misma posición, pero de la lista de valores.
                    -ks no tiene repetidos
        OBSERVACIONES:
                * n hace referencia a la cantidad de elementos DISTINTOS de ks( o vs, es indiferente).
    -}

emptyM :: Map k v                                                                                        -- O(1)
--Propósito: devuelve un map vacío                                                       
----assocM :: Eq k => k -> v -> Map k v -> Map k v                                                           -- O(n)
--Propósito: agrega una asociación clave-valor al map.
--Observacion: Si la clave ya existe en el Map, la sobreescribe, sino crea un nuevo par clave-valor.
----lookUpM :: Eq k => k -> Map k v -> Maybe v                                                               -- O(n)
--Propósito: encuentra un valor dado una clave.
----deleteM :: Eq k => k -> Map k v -> Map k v                                                               -- O(n)
--Propósito: borra una asociación dada una clave.
keys :: Map k v -> [k]                                                                                   -- O(1)
--Propósito: devuelve las claves del map.



emptyM = M [] []



--assocM k v (M ks vs) = if pertenece k ks 
                            --then M ks (agregarValorEn v (indiceDeEn k ks) vs)
                            --else (M (k:ks) (v:vs))

--indiceDeEn :: Eq k => k -> [k] -> Maybe Int                                                              -- O(n)
--indiceDeEn k []      = Nothing
--indiceDeEn k (k':ks) = if k==k'
                            --then Just 0
                            --else (Just 1) + indiceDeEn k ks

--agregarValorEn :: a -> Maybe Int -> [a] -> [a]                                                           -- O(n)
-- PRECOND : La lista no puede hacerse vacia, antes de que el numero llegue a cero.
--agregarValorEn x' 0 (x:xs) = (x':xs)
--agregarValorEn x' n (x:xs) = x : agregarValorEn x' (n-1) xs

pertenece :: Eq a => a -> [a] -> Bool                                                                    -- O(n)
pertenece _ []     = False
pertenece t (x:xs) = (t == x) || pertenece t xs



--lookUpM k (M ks vs) = devolverValorEn (indiceDeEn k ks) vs

--devolverValorEn :: Maybe Int -> [a] -> Maybe a                                                           -- O(n)
--devolverValorEn Nothing xs = Nothing
--devolverValorEn 0 (x:xs)   = Just x
--devolverValorEn n (x:xs)   = devolverValorEn (n-1) xs



--deleteM k (M ks vs) = if pertenece k ks 
                            --then M (sacarDe k ks) (sacarDeEn vs (indiceDeEn k ks))
                            --else (M ks vs)

--sacarDe :: Eq a => a -> [a] -> [a]                                                                       -- O(n)
-- PRECOND : La lista contiene al elemento
--sacarDe x (x':xs) = if x==x'
                        --then xs
                        --else x' : sacarDe x xs

--sacarDeEn :: [a] -> Maybe Int -> [a]                                                                     -- O(n)
-- PRECOND : La lista no puede hacerse vacia, antes de que el numero llegue a cero.
--sacarDeEn (x:xs) 0 = xs
--sacarDeEn (x:xs) n = x : sacarDeEn xs (n-1)



keys (M ks vs) = ks                                                                                      -- O(1)

-- Hay que hacer un indice que devuelva un Maybe Int y un indice que devuelva un Int