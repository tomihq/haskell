data Literal = Lit String [String] 
               deriving (Eq, Show)

data ClausulaDeDefinicion = Def Literal [Literal]
                            deriving (Eq, Show)

type ClausulaObjetivo = [Literal]

esVacia :: ClausulaObjetivo -> Bool
esVacia = (\l -> length l == 0)

resolvente :: ClausulaObjetivo -> ClausulaDeDefinicion -> Maybe ClausulaObjetivo
resolvente (x : xs) (Def k childs) 
    | x == k = Just (childs ++ xs)
    | otherwise = Nothing  

existeRefutacionSLD :: [ClausulaDeDefinicion] -> ClausulaObjetivo -> Bool
existeRefutacionSLD clausulasDef obj 
    | esVacia obj = True 
    | otherwise = any (\clausula -> case resolvente obj clausula of 
                            Nothing -> False 
                            (Just newObj) -> existeRefutacionSLD clausulasDef newObj           
                      ) clausulasDef
