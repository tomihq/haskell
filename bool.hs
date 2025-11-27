-- quiero hacer un sistema que cubra booleanos y pueda evaluarlos.
-- Sea un término True OR True deberia dar TRUE.

data BoolM = TrueM | FalseM | AndM BoolM BoolM | OrM BoolM BoolM | NotM BoolM deriving Show

notM :: BoolM -> BoolM
notM TrueM = FalseM
notM FalseM = TrueM

andM :: BoolM -> BoolM -> BoolM 
andM TrueM TrueM = TrueM
andM FalseM _ = FalseM 
andM TrueM _ = FalseM 

orM:: BoolM -> BoolM -> BoolM 
orM TrueM _ = TrueM 
orM _ TrueM = TrueM 

xorM :: BoolM -> BoolM -> BoolM 
xorM TrueM FalseM = TrueM
xorM FalseM TrueM = TrueM
xorM _ _ = FalseM 

eqM :: BoolM -> BoolM -> BoolM 
eqM TrueM TrueM = TrueM 
eqM FalseM FalseM = TrueM 
eqM _ _ = FalseM 

foldBoolM :: b -> b -> (b -> b -> b) -> (b -> b -> b) -> (b -> b) -> BoolM -> b
foldBoolM t f andf orf notf input = case input of
    TrueM -> t
    FalseM -> f 
    NotM a -> notf (rec a)
    AndM a b -> andf (rec a) (rec b)
    OrM a b -> orf (rec a) (rec b) 
    where rec = foldBoolM t f andf orf notf