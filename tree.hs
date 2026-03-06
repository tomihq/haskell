mapFoldl f = foldl (\ac x -> f x:ac) []  
-- (1:[], 2:[1], [2, 1], 3:[2, 1], [3, 2, 1]) -- (([] estrella 1) estrella 2) estrella 3 -- reemplazando estrella por la operacion rec f 3:(f 2:(f 1:[]))). Da los resultados al revés! 
mapFoldlOrdenado f = foldl(\ac x -> ac ++ [f x]) []  
-- ([] ++ [1], [1] ++ [2], [1, 2] ++ [3], [1, 2, 3])

