module FinRec.Exp where

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

data Program = Program { programName :: String, programBody :: [Exp]}

string :: String -> Exp
string = Literal . String

bool :: Bool -> Exp
bool = Literal . Bool

int :: Int -> Exp
int = Literal . Number . fromIntegral

float :: Float -> Exp
float = Literal . Number

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
    show (Program n es) =
        n ++ ":\n" ++ intercalate "\n" (map show es)
