module FinRec.DefaultContext 
( valueContext
, typeContext
) where

import FinRec.Context
import FinRec.Type
import FinRec.Val

import Data.Fixed ( mod' )
    
tern :: Bool -> a -> a -> a
tern b x y = if b then x else y

defaultContext :: [(String, [Val] -> Val, [Type], Type)]
defaultContext = 
    [ prim (num_    $ num_    num   ) "plus" (+)
    , prim (num_    $ num_    num   ) "minus" (-)
    , prim (num_    $ num_    num   ) "times" (*)
    , prim (num_    $ num_    num   ) "div" (/)
    , prim (num_    num   ) "abs" abs
    , prim (num_    num   ) "sign" signum

    , prim (tv_ 0   $ tv_ 0   bool  ) "eq"  (==)
    , prim (tv_ 0   $ tv_ 0   bool  ) "leq" (<=)

    , prim (bool_   $tv_ 0 $ tv_ 0 $ tv 0) "tern" tern

    , prim (bool_   $ bool_   bool  ) "and" (&&)
    , prim (bool_   $ bool_   bool  ) "or" (||)
    , prim (bool_   bool  ) "not" not
    , prim (num_    num   ) "round" (fromIntegral . round)
    , prim (num_    num   ) "floor" (fromIntegral . floor)
    , prim (num_    num   ) "ceil" (fromIntegral . ceiling)
    , prim (num_    $ num_    num   ) "mod" mod'
    , prim num   "pi" pi
    , prim (num_    num   ) "exp" exp
    , prim (num_    num   ) "log" log
    , prim (num_    num   ) "sin" sin
    , prim (num_    num   ) "cos" cos
    , prim (num_    num   ) "asin" asin
    , prim (num_    num   ) "acos" acos
    , prim (num_    num   ) "sinh" sinh
    , prim (num_    num   ) "cosh" cosh
    , prim (num_    num   ) "asinh" asinh
    , prim (num_    num   ) "acosh" acosh
    , prim (num_    num   ) "atanh" atanh

    , prim (string_ $ tv 0  ) "read"   read
    , prim (tv_ 0   string) "tostring" show
    , prim (string_ $ string_ string) "append" (++)
    , prim (num_    $ string_ string) "take" (\ i -> take (floor i))
    , prim (num_    $ string_ string) "drop" (\ i -> drop (floor i))
    , prim (string_ num   ) "length" (fromIntegral . length)
    , prim (tv_ 0   $ tv 0) "id" id
    , prim (tv_ 0   $ tv_ 1   $ tv 1   ) "const" const
    ]

valueContext :: Context
valueContext = map (\ (op, f, _, _) -> (op, f)) defaultContext 

typeContext :: PolyTypeContext
typeContext = map (\ (op, _, ts, t) -> (op, quantifyType $ funType ts t)) defaultContext 

data TypeDict a =
    TypeDict {
        denotation :: a -> [Val] -> Val,
        numypes :: [Type],
        outType :: Type
    }

base :: (t -> Val) -> Type -> TypeDict t
base c t =
    TypeDict {
        denotation = \ x [] -> c x,
        numypes = [],
        outType = t
    }

fun :: (Val -> t) -> Type -> TypeDict a -> TypeDict (t -> a)
fun d t d0 =
    TypeDict {
        denotation = \ f (v:vs) -> denotation d0 (f (d v)) vs,
        numypes = t : numypes d0,
        outType = outType d0
    }

num :: TypeDict Float
num = base Number numType

bool :: TypeDict Bool
bool = base Bool boolType

string :: TypeDict String
string = base String stringType

tv :: Int -> TypeDict Val
tv = base id . TVar

num_    :: TypeDict a -> TypeDict (Float -> a)
num_    = fun (\ (Number n) -> n) numType

bool_   :: TypeDict a -> TypeDict (Bool -> a)
bool_   = fun (\ (Bool n) -> n) boolType

string_ :: TypeDict a -> TypeDict (String -> a)
string_ = fun (\ (String n) -> n) stringType

tv_ :: Int -> TypeDict a -> TypeDict (Val -> a)
tv_ i = fun id (TVar i)

prim ::TypeDict a -> String -> a -> (String, [Val] -> Val, [Type], Type)
prim d op f = (op, denotation d f, numypes d, outType d)
