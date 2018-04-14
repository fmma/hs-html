module FinRec.Val where

import FinRec.Type
import FinRec.Runtime

import Data.List ( intercalate )

data Val = String String
    | Bool Bool
    | Number Float
    | Tuple [Val]
    | TypeVal Type
    deriving (Eq, Ord, Read)

typeof :: Val -> Type
typeof v =
    case v of
        String _ -> stringType
        Bool _ -> boolType
        Number _ -> numType
        Tuple vs -> tupleType (map typeof vs)
        TypeVal t -> typeType t

project :: Int -> Val -> Val
project i v =
    case matchTuple v of
        Just vs | i >= 0 && i < length vs -> vs !! i
        _ -> runtimeError "bad tuple projection"

matchTypeVal :: Val -> Maybe Type
matchTypeVal v =
    case v of
        TypeVal t -> Just t
        Tuple ts -> tupleType <$> traverse matchTypeVal ts
        _ -> Nothing

matchTuple :: Val -> Maybe [Val]
matchTuple v =
    case v of
        Tuple vs -> Just vs
        TypeVal t | matchTupleType t == Just [] -> Just []
        _ -> Nothing

instance Show Val where
    show v =
        case v of
            String xs -> show xs
            Number n -> show n
            Bool b -> show b
            Tuple vs -> "(" ++ intercalate ", " (map show vs) ++ ")"
            TypeVal t -> show t