module MultiSetV1 (MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList) where

import MapV1 (Map, emptyM, assocM, lookUpM, deleteM, keys)
-- Soy implementador de MultiSet, pero usuario de Map.

data MultiSet a = MS (Map a Int)
    {-INV.REP.:
                *Cada par clave-valor del Map, representa a un elemento del Multiset y su cantidad de apariciones.
    -}

emptyMS :: MultiSet a                                                                              -- O(1)
--Propósito: denota un multiconjunto vacío.
addMS :: Ord a => a -> MultiSet a -> MultiSet a                                                    -- O(n)
--Propósito: dados un elemento y un multiconjunto, agrega una ocurrencia de ese elemento al
--multiconjunto.
ocurrencesMS :: Ord a => a -> MultiSet a -> Int                                                    -- O(n)
--Propósito: dados un elemento y un multiconjunto indica la cantidad de apariciones de ese
--elemento en el multiconjunto.
unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a                                         -- O(n^2)
--Propósito: dados dos multiconjuntos devuelve un multiconjunto con todos los elementos de
--ambos multiconjuntos.
intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a                                  -- O(n^2)
--Propósito: dados dos multiconjuntos devuelve el multiconjunto de elementos que ambos
--multiconjuntos tienen en común.
multiSetToList :: Eq a => MultiSet a -> [(a, Int)]                                                 -- O(n^2)
--Propósito: dado un multiconjunto devuelve una lista con todos los elementos del conjunto y
--su cantidad de ocurrencias.



emptyMS = (MS (emptyM))



addMS x (MS (map)) = (MS (assocM x ((fromJustInventadoNumerico (lookUpM x map)) + 1) map))



ocurrencesMS x (MS (map)) = (fromJustInventadoNumerico (lookUpM x map))



unionMS (MS map1) (MS map2) = (MS (asociarMapAMap map1 map2))

asociarMapAMap :: Eq k => Map k v -> Map k v -> Map k v                                            -- O(n^2)
asociarMapAMap map1 map2 = asociarCadaElementoA (keys (map1)) (listaConTodosLosValues map1) map2

listaConTodosLosValues :: Eq k => Map k v -> [v]                                                   -- O(n^2)
listaConTodosLosValues map = if null (keys map) 
                                then []
                                else (fromJust (lookUpM (head(keys map)) map)) : listaConTodosLosValues (deleteM (head(keys map)) map)

asociarCadaElementoA :: Eq k => [k] -> [v] -> Map k v -> Map k v                                   -- O(n^2), n siendo la cantidad de elementos en ks
-- PRECOND: Si ks es vacia, vs tambien lo es
asociarCadaElementoA []      _     map = map
asociarCadaElementoA (k:ks) (v:vs) map = assocM k v (asociarCadaElementoA ks vs map)



intersectionMS (MS map1) (MS map2) = (MS (asociarSiApareceEnElSegundo map1 map2))

asociarSiApareceEnElSegundo :: Eq k => Map k Int -> Map k Int -> Map k Int                         -- O(n^2)
asociarSiApareceEnElSegundo map1 map2 = asociarSiApareceEnElMap (keys (map1)) (listaConTodosLosValues map1) map2

asociarSiApareceEnElMap :: Eq k => [k] -> [Int] -> Map k Int -> Map k Int                          -- O(n^2), n siendo la cantidad de elementos en ks
-- PRECOND: Si ks es vacia, vs tambien lo es
asociarSiApareceEnElMap []     vs     _    = emptyM
asociarSiApareceEnElMap (k:ks) (v:vs) map2 = if pertenece k (keys map2)
                                                then assocM k (v + fromJustInventadoNumerico(lookUpM k map2))  (asociarSiApareceEnElMap ks vs map2)
                                                else asociarSiApareceEnElMap ks vs map2

pertenece :: Eq a => a -> [a] -> Bool                                                              -- O(n)
pertenece _ []     = False
pertenece t (x:xs) = (t == x) || pertenece t xs



multiSetToList (MS map) = mapToList map

mapToList :: Eq k => Map k v -> [(k, v)]                                                           -- O(n^2)
mapToList map = asociarKeysAValorEn (keys map) map

asociarKeysAValorEn :: Eq k => [k] -> Map k v -> [(k,v)]                                           -- O(n^2), n siendo la cantidad de elementos en ks
-- PRECOND: Todas las keys pertenecen al map.
asociarKeysAValorEn []     map = []
asociarKeysAValorEn (k:ks) map = (k,fromJust (lookUpM k map)) : asociarKeysAValorEn ks map 

fromJustInventadoNumerico :: Maybe Int -> Int
fromJustInventadoNumerico Nothing  = 0
fromJustInventadoNumerico (Just n) = n

fromJust :: Maybe a -> a -- O(1)
fromJust Nothing  = error "No hay elemento"
fromJust (Just a) = a



multiSet1 :: MultiSet Char
multiSet1 = (MS (assocM 'a' 7(assocM 'b' 4 (emptyM))))

multiSet2 :: MultiSet Char
multiSet2 = (MS (assocM 'c' 2(assocM 'd' 5 (emptyM))))