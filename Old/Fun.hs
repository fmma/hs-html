module Fun where 

data Fun a b where
    Constant :: Show a => a -> Fun () a
    Fst :: Fun (a, b) a
    Snd :: Fun (a, b) b
    Plus :: Fun (Int, Int) Int
    TernCond :: Fun (Bool, a, a) a
    Equals :: Fun (a, a) Bool
    Less :: Fun (a, a) Bool
    
    Index :: Fun ([a], Int) a
    Length :: Fun [a] a