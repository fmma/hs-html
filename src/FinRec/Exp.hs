module FinRec.Exp where

import FinRec.Infix
import FinRec.Val

import Data.List ( intercalate )

data Exp
    = Literal Val
    | Apply String [Exp]
    | Index Int
    | Var Int
    | TupleExp [Exp]
    | Project Exp Int
    | Input
    | Refernce String

data Program = Program 
    { programName :: String
    , programBody :: [Exp]
    , programFixity :: Maybe (String, Fixity, Int)
    }

string :: String -> Exp
string = Literal . String

bool :: Bool -> Exp
bool = Literal . Bool

int :: Int -> Exp
int = Literal . Number . fromIntegral

float :: Float -> Exp
float = Literal . Number

plus :: Exp -> Exp -> Exp
plus e1 e2 = Apply "plus" [e1, e2]

minus :: Exp -> Exp -> Exp
minus e1 e2 = Apply "minus" [e1, e2]

times :: Exp -> Exp -> Exp
times e1 e2 = Apply "times" [e1, e2]

divide :: Exp -> Exp -> Exp
divide e1 e2 = Apply "div" [e1, e2]

instance Show Exp where
    show e =
        case e of
            Literal v -> show v
            Apply f es -> f ++ "(" ++ intercalate ", " (map show es) ++ ")"
            Index i -> "i" ++ show i
            Var x -> "x" ++ show x
            TupleExp es -> show (Apply "" es)
            Project e0 i -> show e0 ++ "." ++ show i
            Input -> "input"

instance Show Program where
    show (Program n es _) =
        n ++ ":\n" ++ intercalate "\n" (map show es)
