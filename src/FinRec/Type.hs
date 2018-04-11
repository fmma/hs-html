module FinRec.Type where

import FinRec.Runtime

import Data.Char ( ord, chr )
import Data.List ( intercalate, nub, sort )

data Type
    = TCon String [Type] 
    | TVar Int
    deriving Eq

data PolyType
    = MonoType Type
    | Forall PolyType

monotype :: PolyType -> Type
monotype s =
    case s of
        MonoType t -> t
        Forall s0 -> monotype s0

quantifyType :: Type -> PolyType
quantifyType t =
    let is = sort $ freeTypeVars t
    in foldl (.) id (replicate (length is) Forall) $ MonoType $ quantifyType' (zip is [0..]) t

quantifyType' :: [(Int, Int)] -> Type -> Type
quantifyType' g t =
    case g of
        [] -> t
        (i,j):g0 -> quantifyType' g0 $ substituteTypeVar i (TVar j) t

polyArity :: PolyType -> Int
polyArity s =
    case s of
        MonoType _ -> 0
        Forall s0 -> 1 + polyArity s0

shiftVars :: Int -> Type -> Type
shiftVars i t =
    case t of
        TVar j -> TVar (i + j)
        TCon f ts -> TCon f (map (shiftVars i) ts)

freeTypeVars :: Type -> [Int]
freeTypeVars t =
    case t of
        TVar i -> [i]
        TCon _ ts -> nub $ concatMap freeTypeVars ts 

substituteTypeVar :: Int -> Type -> Type -> Type
substituteTypeVar i t0 t =
    case t of 
        TVar j | i == j -> t0
               | otherwise -> TVar j
        TCon f ts -> TCon f $ map (substituteTypeVar i t0) ts

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

instance Show PolyType where
    show = showPolyType 0

showOpenTupleType :: Type -> [Char]
showOpenTupleType t =
    case t of
        TCon "::" [t0, t1] -> show t0 ++ ", " ++ showOpenTupleType t1
        TVar _ -> ".."
        TCon "NIL" [] -> ""
        _ -> runtimeError $ "Ill-formed open tuple: " ++ show t

showPolyType :: Int -> PolyType -> String
showPolyType i s =
    case s of
        MonoType t -> show t
        Forall s0 -> "forall " ++ show (TVar i) ++ ". " ++ showPolyType (i+1) s0