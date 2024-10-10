import MapV1 (Map, emptyM, assocM, lookUpM, deleteM, keys)

import MultiSetV1 (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList)



-- EJERCICIOS MAP:

valuesM :: Eq k => Map k v -> [Maybe v]                                                            -- O(n^2)
valuesM map = (lookUpM (head (keys map)) map) : valuesM (deleteM (head (keys map)) map)



todasAsociadas :: Eq k => [k] -> Map k v -> Bool                                                   -- O(n^2)
todasAsociadas []     map = True
todasAsociadas (k:ks) map = (pertenece k (keys map)) && todasAsociadas ks map

pertenece :: Eq a => a -> [a] -> Bool                                                              -- O(n)
pertenece _ []     = False
pertenece t (x:xs) = (t == x) || pertenece t xs



listToMap :: Eq k => [(k, v)] -> Map k v                                                           -- O(n^2)
listToMap []          = emptyM
listToMap ((k,v):kvs) = assocM k v (listToMap kvs) 



mapToList :: Eq k => Map k v -> [(k, v)]                                                           -- O(n^2)
mapToList map = asociarKeysAValorEn (keys map) map

asociarKeysAValorEn :: Eq k => [k] -> Map k v -> [(k,v)]                                           -- O(n^2)
-- PRECOND: Todas las keys pertenecen al map.
asociarKeysAValorEn []     map = []
asociarKeysAValorEn (k:ks) map = (k,fromJust (lookUpM k map)) : asociarKeysAValorEn ks map 

fromJust :: Maybe a -> a
fromJust Nothing  = error "No hay elemento"
fromJust (Just a) = a



agruparEq :: Eq k => [(k, v)] -> Map k [v]                                                         -- O(n^2)
agruparEq []          = emptyM
agruparEq ((k,v):kvs) = agruparPorClave k v (agruparEq kvs) 

agruparPorClave :: Eq k => k -> v -> Map k [v] -> Map k [v]                                        -- O(n)
agruparPorClave k v map = case lookUpM k map of
                                Nothing -> assocM k [v] map
                                Just vs -> assocM k (v:vs) map



incrementar :: Eq k => [k] -> Map k Int -> Map k Int                                               -- O(n^2)
incrementar []     map = map
incrementar (k:ks) map = case lookUpM k map of
                            Nothing  -> incrementar ks map
                            Just n   -> assocM k (n + 1) (incrementar ks map)


map1 :: Map Int Int
map1 = assocM 10 8 (assocM 9 7 (emptyM))

map2 :: Map Int Int
map2 = assocM 2 3 (assocM 10 7 (emptyM))



mergeMaps :: Eq k => Map k v -> Map k v -> Map k v                                                  -- O(n^2)
mergeMaps map1 map2 = if esAsociacionSinClaves map1 
                            then map2
                            else agregarClavesDeA map1 map2

esAsociacionSinClaves :: Map k v -> Bool                                                           -- O(n)
esAsociacionSinClaves map = if null (keys map)
                                then True
                                else False

agregarClavesDeA :: Eq k => Map k v -> Map k v -> Map k v                                          -- O(n^2)
agregarClavesDeA map1 map2 = assocM (head (keys map1)) (fromJust (lookUpM (head (keys map1)) map1 )) (mergeMaps (deleteM (head (keys map1)) map1) map2)



indexar :: [a] -> Map Int a                                                                        -- O(n^2)
indexar xs = indexarAPartirDeIndice 0 xs 

indexarAPartirDeIndice :: Int -> [a] ->  Map Int a                                                 -- O(n^2)
indexarAPartirDeIndice _ []     = emptyM 
indexarAPartirDeIndice n (x:xs) = assocM n x (indexarAPartirDeIndice (n+1) xs)

{-
Se puede realizar sin usar el indice y 
haciendo a indexar, una funcion por recursion 
-}



ocurrencias :: String -> Map Char Int                                                              -- O(n^2)
ocurrencias []     = emptyM 
ocurrencias (c:cs) = assocM c ((apariciones c cs) + 1) (ocurrencias cs)

apariciones :: Eq a => a -> [a] -> Int                                                             -- O(n)
apariciones _ []     = 0
apariciones x (y:ys) = unoSiCeroSino (x == y) + apariciones x ys

unoSiCeroSino :: Bool -> Int                                                                       -- O(1)
unoSiCeroSino True = 1
unoSiCeroSino _    = 0



-- EJERCICIOS MULTISET : 

ocurrenciasMS :: String -> MultiSet Char                                                             
ocurrenciasMS []     = emptyMS
ocurrenciasMS (c:cs) = addMS c (ocurrenciasMS cs)