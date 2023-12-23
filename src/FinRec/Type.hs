module FinRec.Type where

import FinRec.Runtime

import Data.Char ( ord, chr )
import Data.List ( intercalate )

data Type
    = TCon String [Type]
    | TVar Int
    deriving (Eq, Ord, Read)

data ProgType = ProgType Int Type [(String, Type)]

baseType :: String -> Type
baseType x = TCon x []

stringType :: Type
stringType = baseType "string"

numType :: Type
numType = baseType "number"

boolType :: Type
boolType = baseType "bool"

tupleType :: [Type] -> Type
tupleType = TCon "()"

typeType :: Type -> Type
typeType t = TCon "type" [t]

openTupleType :: [Type] -> Type
openTupleType ts = 
    case ts of
        [] -> runtimeError "No type var for open tuple remainder"
        [t] -> t
        t:ts0 -> TCon "::" [t, openTupleType ts0]

matchTupleType :: Type -> Maybe [Type]
matchTupleType t =
    case t of
        TCon "()" ts -> Just ts
        _ -> Nothing

funType :: [Type] -> Type -> Type
funType ts t = TCon "->" (ts ++ [t])

matchFunType :: Type -> Maybe ([Type], Type)
matchFunType t =
    case t of
        TCon "->" ts@(_:_) -> Just (init ts, last ts)
        _ -> Nothing

instance Show Type where
    show (TCon t []) = t
    show (TCon "->" ts) = show (TCon "()" (init ts)) ++ " -> " ++ show (last ts)
    show (TCon "()" ts) = "(" ++ intercalate ", " (map show ts) ++ ")"
    show (TCon "::" ts) = "(" ++ showOpenTupleType (TCon "::" ts) ++ ")"
    show (TCon t ts) = t ++ "(" ++ intercalate ", " (map show ts) ++ ")"
    show (TVar i) = 
        if i + ord 'a' <= ord 'z'
            then [chr $ i + ord 'a']
            else "z" ++ show (i - (ord 'z' - ord 'a'))

showOpenTupleType :: Type -> [Char]
showOpenTupleType t =
    case t of
        TCon "::" [t0, t1] -> show t0 ++ ", " ++ showOpenTupleType t1
        TVar _ -> ".."
        TCon "NIL" [] -> ""
        _ -> runtimeError $ "Ill-formed open tuple: " ++ show t

