mapFoldl f = foldl (\ac x -> f x:ac) []  
-- (1:[], 2:[1], [2, 1], 3:[2, 1], [3, 2, 1]) -- ((z estrella 1) estrella 2) estrella 3 -- reemplazando estrella por la operacion rec f 3:(f 2:(f 1:[]))). Da los resultados al revés! 
mapFoldlOrdenado f = foldl(\ac x -> ac ++ [f x]) []  
-- ([] ++ [1], [1] ++ [2], [1, 2] ++ [3], [1, 2, 3])

mapFoldr f = foldr(\r rec -> f r : rec) []
--(a estrella (b estrella (c estrella z))) -- (1 estrella (2 estrella (3 estrella []))) -- reemplazando estrella por operación: 1:(2:(3:[]))

-- Denotacionalmente parece que mapFoldr devuelve lo mismo que mapFoldlOrdenado pues observemos que take 2 (mapFoldr (*2) [1,2,undefined]) y take 2 (mapFoldlOrdenado (*2) [1,2,undefined]) dan lo mismo. 
-- En este caso termina porque mapFoldlOrdenado está manejando una lista finita, y solo se toman los dos primeros elementos.
-- ¿Por qué pasó esto? ¿Por qué al caer en undefined con listas pequeñas no estalló? porque el take nos estaba salvando de llegar a ejecutar. El fold "produce" la estructura, y solo ejecuta lo que se necesita.
-- Ej.: foldr haría take 2 (pasoCon1, pasoCon2) foldl haría take 2 (pasoCon1, pasoCon2, pasoConN) sin ejecutar. Luego el take agarra lo que necesita, y evalúa. En ambos casos no pasaba nada, porque el foldl operaba con listas pequeñas. Pero, al momento de tener que plegar la operación infinitamente se muere. 
-- Si probamos take 5 (mapFoldr (*2) [1..]) vemos que foldr responde correctamente dando los 5 primeros números pero take 5 (mapFoldlOrdenado (*2) [1..]) se cuelga porque nunca termina de preparar infinitas operaciones. 
