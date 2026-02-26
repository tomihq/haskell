data Literal = Lit String [String] 
               deriving (Eq, Show)

data ClausulaDeDefinicion = Def Literal [Literal]
                            deriving (Eq, Show)

type ClausulaObjetivo = [Literal]

esVacia :: ClausulaObjetivo -> Bool
esVacia = (\l -> length l == 0)

resolvente :: ClausulaObjetivo -> ClausulaDeDefinicion -> ClausulaObjetivo
resolvente (x:xs) (Def k childs)
    | x == k  = childs ++ xs
    | otherwise = x:xs

-- Nota: la resolución SLD se puede colgar por la propia naturaleza de que al hacer DFS puede caer en una situación ad-infinitum. Es un tradeoff entre correctitud y eficiencia. La resolución SLD es lineal, binaria, empezás con cláusula objetivo y tenés cláusulas de Horn.

existeRefutacionSLD :: [ClausulaDeDefinicion] -> ClausulaObjetivo -> Bool
existeRefutacionSLD defs obj
  | esVacia obj = True
  | otherwise =
      any (\d ->
            let newObj = resolvente obj d
            in newObj /= obj &&
               existeRefutacionSLD defs newObj
          ) defs
