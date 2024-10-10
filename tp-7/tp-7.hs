-- PUNTO 1 :
-- Tenes que insertar cada uno de los elementos en una PQ(en una subtarea), o sea que tendrias operaciones de costo 0(log(n)) por cada elemento,
-- o sea, la funcion tendria un costo 0(n.log(n)), siendo n el largo de la lista.

-- PUNTO 2 : Hecho en papel

-- PUNTO 3 :

-- 1) valuesM        = O(K^2), porque por cada elemento pido todas las keys.
-- 2) todasAsociadas = O(K.n), siendo n el largo de la lista y K todos los elementos del Map.
-- 3) listToMap      = O(log(K).n), siendo n el largo de la lista y K todos los elementos del Map.
-- 4) mapToList      = O(log(K).n), siendo n el largo de la lista y K todos los elementos del Map.
-- 5) agruparEq      = O(log(K).n), siendo n el largo de la lista y K todos los elementos del Map.
-- 6) incrementar    = O(log(K).n), siendo n el largo de la lista y K todos los elementos del Map.
-- 7) mergeMaps      = O(K^2), porque por cada elemento pido todas las keys.
-- 8) indexar        = O(log(K).n), siendo n el largo de la lista y K todos los elementos del Map.
-- 9) ocurrencias    = O(n^2), siendo n el largo de la lista, porque por cada elemento de la lista, pregunta por sus apariciones.

-- PUNTO 5 : Hecho en papel