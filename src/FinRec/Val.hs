module FinRec.Val where

import FinRec.Type
import FinRec.Runtime

import Data.List ( intercalate )

data Val = String String
    | Bool Bool
    | Number Float
    | Tuple [Val]
    deriving (Eq, Ord, Read)

typeof :: Val -> Type
typeof v =
    case v of
        String _ -> stringType
        Bool _ -> boolType
        Number _ -> numType
        Tuple vs -> tupleType (map typeof vs)

project :: Int -> Val -> Val
project i v =
    case v of
        Tuple vs | i >= 0 && i < length vs -> vs !! i
        _ -> runtimeError "bad tuple projection"

instance Show Val where
    show v =
        case v of
            String xs -> show xs
            Number n -> show n
            Bool b -> show b
            Tuple vs -> "(" ++ intercalate ", " (map show vs) ++ ")"
