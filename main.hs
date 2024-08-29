{-
problema f (n: Z) : Z {
 requiere: {n = 1 ∨ n = 4 ∨ n=16}
 asegura: {(n = 1 → result = 8) ∧ (n = 4 → result = 131) ∧ (n = 16 → result = 16)}
 }

Análogamente, especificar e implementar la funci´on parcial g :: Integer->Integer
 g(8) = 16
 g(16) = 4
 g(131) = 1

A partir de las funciones definidas en los items 1 y 2, implementar las funciones parciales h = f ◦ g y k = g ◦f

-}

f :: Integer -> Integer 
f 1 = 8
f 4 = 131 
f 16 = 16

g :: Integer -> Integer 
g 8 = 16 
g 16 = 4
g 131 = 1

h :: Integer -> Integer 
h = f . g

k :: Integer -> Integer
k = g . f

{-
Ejercicio 2. Especificar e implementar las siguientes funciones, incluyendo su signatura.
-}

absoluto :: (Num a, Ord a) => a -> a 
absoluto n | (>0) n = n
           | otherwise = -n

maximoAbsoluto :: (Num a, Ord a) => a -> a -> a
maximoAbsoluto n m | absoluto n > absoluto m = absoluto n
                   | otherwise = absoluto m

maximo3 :: (Num a, Ord a) => a -> a -> a -> a 
maximo3 n m r | (>) n m && (>) n r = n 
              | (>) m n && (>) m r = m 
              | otherwise = r

algunoEs0 :: (Num a, Eq a) => a -> a -> Bool
algunoEs0 0 _ = True
algunoEs0 _ 0 = True 
algunoEs0 n m = False

ambosSonCero :: (Num a, Eq a) => a -> a -> Bool
ambosSonCero 0 0 = True 
ambosSonCero _ _ = False

mismoIntervalo :: (Num a, Ord a) => a -> a -> Bool
mismoIntervalo n m | n <= 3 && m <= 3 = True 
                   | (n > 3 && n<=7) && (m>3 && m<=7) = True
                   | (n > 7) && (m>7) = True
                   | otherwise = False

esMultiploDe :: (Integral a, Ord a) => a -> a -> Bool
esMultiploDe n = (== 0) . mod n 

prodInt :: (Num a) => (a, a) -> (a, a) -> a
prodInt t1 t2 = (+) (fst t1 * fst t2) (snd t1 * snd t2)

todoMenor :: (Num a, Ord a) => (a, a) -> (a, a) -> Bool
todoMenor t1 t2 = fst t1 < fst t2 && snd t1 < snd t2

{- sqrt((x2 - y2)^2 ] + (x1 - y1)^2)-}
distanciaPuntos :: (Floating a) => (a, a) -> (a, a) -> a
distanciaPuntos t1 t2 = sqrt ((snd t2 - snd t1)^2 + (fst t1 - fst t2)^2)

crearPar :: a -> b -> (a, b)
crearPar a b = (a, b)

invertir :: (a, b) -> (b, a)
invertir (a, b) = (b, a)

fibonacci :: Integer -> Integer 
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci(n-1) + fibonacci(n-2)

{-Sin usar mod ni div -}
esDivisible :: Integer -> Integer -> Bool
esDivisible n m | n < 0     = False
                | n == 0    = True
                | otherwise = esDivisible (n - m) m

medioFact :: Integer -> Integer
medioFact n | n <= 0 = 1
            | otherwise = n * medioFact (n-2) 

sumaDigitos :: Integer -> Integer 
sumaDigitos n | n == 0 = 0 
              | otherwise = mod n 10 + sumaDigitos (mod (n-1) 10)

longitud :: [t] -> Integer
longitud [] = 0
longitud (x:xs) = 1 + longitud xs

longitudFoldr :: [t] -> Integer
longitudFoldr = foldr (\x acc -> acc + 1) 0

ultimo :: [t] -> t 
ultimo [x] = x 
ultimo (x:xs) = ultimo xs

reverso :: [t] -> [t]
reverso [] = []
reverso (x:xs) = (++) (reverso xs) [x]

pertenece :: (Eq t) => t -> [t] -> Bool
pertenece e l | longitud l == 0 = False
              | e == (head l) = True 
              | otherwise = pertenece e (tail l)

todosIguales :: (Eq t) => [t] -> Bool
todosIguales l   | longitud l == 0 || longitud l == 1 = True 
                 | (head l) /= (head (tail l)) = False
                 | otherwise = todosIguales (tail l)

{-devuelvo la cola de la lista apenas veo el elemento-}
quitar :: (Eq t) => t -> [t] -> [t]
quitar _ [] = []  
quitar e (x:xs)
    | e == x = xs  
    | otherwise = x : quitar e xs  

{-armo una nueva lista-}
quitarTodos :: (Eq t) => t -> [t] -> [t]
quitarTodos _ [] = []
quitarTodos e (x:xs)
                    | e == x = quitarTodos e xs
                    | otherwise = x : quitarTodos e xs

{- a -> Bool es la firma de /=n -}
quitarTodosFilter :: (a -> Bool) -> [a] -> [a]
quitarTodosFilter f = filter f

