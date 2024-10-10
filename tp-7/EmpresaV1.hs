-- PUNTO 4 :
-- import SetV1 (Set, emptyS, addS, belongs, sizeS, removeS, unionS, setToList)
-- import MapV1 (Map, emptyM, assocM, lookUpM, deleteM, keys)

module EmpresaV1 (Empresa, consEmpresa, buscarPorCUIL, empleadosDelSector, todosLosCUIL, todosLosSectores, agregarSector, agregarEmpleado, agregarASector, borrarEmpleado) where 

type SectorId = Int
type CUIL = Int

data Empresa = ConsE    (Map SectorId (Set Empleado)) -- MapSectores
                        (Map CUIL Empleado)           -- MapEmpleados
    {- Inv.Rep.:
            * Todos los empleados del MapEmpleados estan asignados al menos a un sector en el MapSectores(si no, estarian en la empresa pero sin trabajar).
            * No puede haber dos empleados asociados al mismo CUIL en MapEmpleados(creo que igual esto ya viene como INV. de REP. de los Maps).
            * No puede haber ni un empleado en ningun Set de empleados en MapSectores, que no este asociado a algun CUIL en el MapEmpleados.
            * Ningun empleado puede trabajar en sectores que no esten en MapSectores.
            * Cada CUIL de cada empleado tiene que estar asociado al mismo CUIL en el el MapEmpleados.
    -}

consEmpresa :: Empresa
--Propósito: construye una empresa vacía.
--Costo: O(1)
buscarPorCUIL :: CUIL -> Empresa -> Empleado
--Propósito: devuelve el empleado con dicho CUIL.
--Precondición: el CUIL es de un empleado de la empresa.
--Costo: O(log E)
empleadosDelSector :: SectorId -> Empresa -> [Empleado]
--Propósito: indica los empleados que trabajan en un sector dado.
--Costo: O(log S + E)
todosLosCUIL :: Empresa -> [CUIL]
--Propósito: indica todos los CUIL de empleados de la empresa.
--Costo: O(E)
todosLosSectores :: Empresa -> [SectorId]
--Propósito: indica todos los sectores de la empresa.
--Costo: O(S)
agregarSector :: SectorId -> Empresa -> Empresa
--Propósito: agrega un sector a la empresa, inicialmente sin empleados.
--Costo: O(log S)
agregarEmpleado :: [SectorId] -> CUIL -> Empresa -> Empresa
--Propósito: agrega un empleado a la empresa, que trabajará en dichos sectores y tendrá el CUIL dado.
--Costo: calcular.
agregarASector :: SectorId -> CUIL -> Empresa -> Empresa
--Propósito: agrega un sector al empleado con dicho CUIL.
--Costo: calcular.
borrarEmpleado :: CUIL -> Empresa -> Empresa
--Propósito: elimina al empleado que posee dicho CUIL.
--Costo: calcular.



consEmpresa = ConsE (emptyM) (emptyM)                                                              -- O(1)



buscarPorCUIL n (ConsE ms me) = fromJust (lookUpM n me)                                            -- O(log(E))



empleadosDelSector n (ConsE ms me) = setToList(fromJust(lookUpM n ms))                             -- O(log(E)+log(S))



todosLosCUIL (ConsE ms me) = keys me                                                               -- O(E)



todosLosSectores (ConsE ms me) = keys ms                                                           -- O(S)



agregarSector n (ConsE ms me) = (ConsE (assocM n emptyS ms) me)                                    -- O(log(S))



agregarEmpleado sis n e = agregarCuilYEmpleadoAEmpresa n (empleadoConCuilYQueTrabajaEnSectores n sis) (agregarEmpleadoASectores sis n e)        -- O((log(S)+log(e)+log(S)).n), siendo n la lista de sectores.


agregarEmpleadoASectores :: [SectorId] -> CUIL -> Empresa -> Empresa                                                                            -- O((log(S)+log(e)+log(S)).n), siendo n la lista de sectores.
agregarEmpleadoASectores [] n e       = agregarCuilAEmpresa n e 
agregarEmpleadoASectores (si:sis) n e = agregarEmpleadoASectores sis n (agregarEmpleadoASector si (empleadoConCuilYQueTrabajaEnSectores n (si:sis)) e)

empleadoConCuilYQueTrabajaEnSectores :: CUIL -> [SectorId] -> Empleado                                                                          -- O(n.log(S)), siendo n la lista de sectores.
empleadoConCuilYQueTrabajaEnSectores n []       = consEmpleado n
empleadoConCuilYQueTrabajaEnSectores _ (si:sis) = incorporarSector si (empleadoConCuilYQueTrabajaEnSectores n sis)

agregarEmpleadoASector :: SectorId -> Empleado -> Empresa -> Empresa                                                                            -- O(log(S)+log(e)+log(S))
agregarEmpleadoASector n empleado e = if perteneceSectorAEmpresa n e
                                        then agregarEmpleadoASectorExistente n empleado e 
                                        else agregarEmpleadoASectorExistente n empleado (agregarSector n e)

perteneceSectorAEmpresa :: SectorId -> Empresa -> Bool                                                                                          -- O(log(S))
perteneceSectorAEmpresa n (ConsE ms me) = case (lookUpM n ms) of
                                            Nothing -> False
                                            Just v  -> True

agregarEmpleadoASectorExistente :: SectorId -> Empleado -> Empresa -> Empresa                                                                   -- O(log(S)+log(e)+log(S))
agregarEmpleadoASectorExistente n empleado (ConsE ms me) = (ConsE (agregarEmpleadoASectorCambiandoElMap n empleado ms) me)

agregarEmpleadoASectorCambiandoElMap :: SectorId -> Empleado -> Map SectorId CUIL -> Map SectorId CUIL                                          -- O(log(S)+log(e)+log(S))
agregarEmpleadoASectorCambiandoElMap n empleado map = assocM n (addS empleado (fromJust(lookUpM n map))) map


agregarCuilYEmpleadoAEmpresa :: CUIL -> Empleado -> Empresa -> Empresa                                                                          -- O(log(E))
agregarCuilAEmpresa n empleado (ConsE ms me) = (ConsE ms (agregarCuilAMapEmpleados n empleado me))

agregarCuilAMapEmpleados :: CUIL -> Empleado -> Map CUIL Empleado -> Map CUIL Empleado                                                          -- O(log(E))
agregarCuilAMapEmpleados n empleado map = assocM n empleado map



agregarASector n m (ConsE ms me) = (ConsE (agregarEmpleadoASector n (empleadoConCuilYQueTrabajaEnSectores m ([n]:(sectores(empleadoConCuil m me))) ms) (agregarCuilAMapEmpleados m (incorporarSector n(empleadoConCuil m me)) me)) )  -- O(log(S)+log(e)+log(S))

empleadoConCuil :: CUIL -> Map CUIL Empleado -> Empleado -- O(log(E))
empleadoConCuil n map = case (lookUpM n map) of
                                Nothing -> (consEmpleado)
                                Just v  -> v



borrarEmpleado n (ConsE ms me) = (ConsE (borrarEmpleadoDeSectores (empleadoConCuil n) ms) (borrarEmpleadoDeMapEmpleados n me)) -- O(n.(log(E).2)), siendo n la lista de sectores donde trabaja el empleado a borrar.


borrarEmpleadoDeSectores :: Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)                             -- O(n.(log(E).2)), siendo n la lista de sectores
borrarEmpleadoDeSectores e map = borrarEmpleadoDeSectoresRercursion (sectores e) e map

borrarEmpleadoDeSectoresRercursion :: [SectorId] -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)     -- O(n.(log(E).2)), siendo n la lista de sectores
borrarEmpleadoDeSectoresRercursion []       e map = map
borrarEmpleadoDeSectoresRercursion (si:sis) e map = borrarEmpleadoDeSector si e (borrarEmpleadoDeSectoresRercursion sis map)

borrarEmpleadoDeSector :: SectorId -> Empleado -> Map SectorId (Set Empleado) -> Map SectorId (Set Empleado)                    -- O(log(E).2)
borrarEmpleadoDeSector si map = assocM si (removeS e (lookUpM si map)) map


borrarEmpleadoDeMapEmpleados :: CUIL -> Map CUIL Empleado -> Map CUIL Empleado                                                 -- O(log(E))
borrarEmpleadoDeMapEmpleados c map = deleteM c map