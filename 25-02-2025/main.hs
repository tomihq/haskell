import Data.List (nub)
import qualified Data.Bits as False
data Form = Prop String | And Form Form | Or Form Form | Neg Form deriving Show

foldForm :: (String -> b) -> (b -> b -> b) -> (b -> b -> b) -> (b -> b) -> Form -> b
foldForm fProp fAnd fOr fNeg form = case form of
        (Prop s) -> fProp s
        (And p q) -> fAnd (rec p) (rec q)
        (Or p q) -> fOr (rec p) (rec q)
        (Neg p) -> fNeg (rec p)
    where rec = foldForm fProp fAnd fOr fNeg

-- foldForm (\s -> Prop s) (\p q  -> And p q) (\p q -> Or p q) (\p -> Neg p) (And(Prop "1") (Prop "2"))
-- fAnd (foldForm fProp fAnd fOr fNeg (Prop "1")) (foldForm fProp fAnd fOr fNeg (Prop "2")) 
-- (\p q  -> And p q) (foldForm fProp fAnd fOr fNeg (Prop "1")) (foldForm fProp fAnd fOr fNeg (Prop "2")) 
-- Beta Rule: (\q  -> And (foldForm fProp fAnd fOr fNeg (Prop "1")) q)
-- Beta Rule: And (foldForm fProp fAnd fOr fNeg (Prop "1")) (foldForm fProp fAnd fOr fNeg (Prop "2")) 
-- And ((\s -> Prop s) "1") ((\s -> Prop s) "2")
-- Beta Rule: And (Prop "1") ((\s -> Prop s) "2")
-- Beta Rule: And (Prop "1") (Prop "2")
-- Result And (Prop "1") (Prop "2") is Form 

-- We had a problem before, that, we tried to use foldForm Const. See what it make it fail.
-- And (Const "1") (Const "2")
-- And "1" "2" -> either "1" or "2" are Form. They're just Strings. 


-- getPropKeys: And (Prop "1") (Prop "2") = ["1", "2"]
-- We need to return a list. Then, the result (type b) should be a [String]. 
-- What does that mean? The base case should return a [String], and, the recursive results of each function of foldForm, should have already the data with [String] form: (\sp sq -> sp ++ sq)

propKeys :: Form -> [String] -- 
propKeys = foldForm (:[]) (++) (++) id

-- let's get a better code!
-- (\p -> p). See fNeg (rec p), if we found a fNeg we should just do the recursive call, or I mean, get the result of the recursive call. That's exactly id something that is same as (\p -> p) (rec p) - beta - rec p = and is exactly the same as id rec p.
-- (\s -> [s]) "1" 
-- Beta: ["1"] but is exactly the same as :[] because (:[])"1" = ["1"]

propsAmount :: (Num a) => Form -> a
propsAmount = foldForm (const 1) (+) (+) id

height :: (Num a, Ord a) =>  Form -> a
height = foldForm (const 1) (\hp hq -> 1 + max hp hq) (\hp hq -> 1+ max hp hq) (1+)

andsAmount :: (Num a, Ord a) =>  Form -> a
andsAmount = foldForm (const 0) (\hp hq -> 1 + hp + hq) (\_ _ -> 0) id

showF :: Form -> String
showF = foldForm id (\ps qs -> "(" ++ ps ++ " ^ " ++ qs ++ ")") (\ps qs -> "(" ++ ps ++ " ∨ " ++ qs ++ ")") ("¬ " ++)

onlyVariables :: Form -> String
onlyVariables = foldForm id (++) (++) id

eval :: Form -> [String] -> Bool
eval = foldForm elem (\pr qr list -> pr list && qr list) (\pr qr list -> pr list || qr list) (\r list  -> not (r list))

-- why l list / r list
-- let's see the base case. The base case is (\s list -> elem s list): Form -> [String] -> Bool = (String -> ([String] -> Bool)) based in foldForm
-- what happens with (\pr qr list -> pr && qr)? pr & qr are the result of the base case: [String] -> Bool. So, if you want it to work, you just need to send the list to get the result.

variables :: Form -> [String]
variables = nub . foldForm (:[]) (++) (++) id

-- Neg(Neg P) = Neg P
clearNegations :: Form -> Form
clearNegations formula = foldForm
    (\s b -> if b then Neg (Prop s) else Prop s) -- Si el padre le mandó un "true" significa: te tenés que negar.
    (\pr qr b -> if b then Neg (And (pr False) (qr False)) else And (pr False) (qr False))
    (\pr qr b -> if b then Neg (Or (pr False) (qr False)) else Or (pr False) (qr False))
    (\p b -> if b then p False else p True) formula False
--Notar que acá el truco de este es que como necesitamos un "booleano" para acumular el estado, y la firma de la función no lo tiene tenemos que definir un valor default nosotros sí o sí. 

-- b: (String, String) -> Form 
-- notar que acá no tenemos que mandar nada por default, porque tenemos los datos enteros por parámetro (String, String) a diferencia del anterior que ese booleano que tenemos que inyectar como argumento nuevo.
replace :: Form -> (String, String) -> Form
replace = foldForm
    (\s (replace, replacement) -> if s == replace then Prop replacement else Prop s)
    (\pr qr replacements -> And (pr replacements) (qr replacements))
    (\pr qr replacements -> Or (pr replacements) (qr replacements))
    (\pr replacements -> Neg (pr replacements))

-- Pasa una formula x a forma normal negada si el booleano es True y pasa a la negacion de x a forma normal negada si el booleano es False.
-- Si b = true entonces en el caso AND y OR dejamos los mismos constructores, pero tenemos que "emitir" si nos midieron NO FNN o FNN.
fnn :: Form -> Bool -> Form
fnn =   foldForm (\s b ->if b then Prop s else Neg (Prop s))
                 (\pr qr b -> if b then And (pr True)  (qr True) else Or  (pr False) (qr False))
                 (\pr qr b -> if b then Or  (pr True)  (qr True) else And (pr False) (qr False))
                 (\pr b -> pr (not b))


foldu :: b -> (c -> b -> b) -> [b -> c] -> b
foldu z f [] = z
foldu z f (x:xs) = f (x (foldu z f xs)) (foldu z f xs)

-- foldr :: (a -> b -> b) -> b -> [a] -> b
-- foldr f z [] = z
-- foldr f z (x:xs)  = f x (foldr f z xs)

-- Escribir foldr con foldu.
-- a de foldr = c de foldu -> foldr (c -> b -> b) -> b -> [? -> c]
-- cambian los casos base de lugar, foldu lo tiene primero.
-- lo que cambia es que foldr me manda una lista digamos, y yo a foldu le tengo que mandar una funcion con dos parametros que ignore la primera, y devuelva solo la lista que me manda foldr.

foldrOwn :: (a -> b -> b) -> b -> [a] -> b
foldrOwn f z l = foldu z f (map const l)
