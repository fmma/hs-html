module FinRec.PolyType where

import FinRec.Type

import Data.List ( nub, sort )

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

instance Show PolyType where
    show = showPolyType 0

showPolyType :: Int -> PolyType -> String
showPolyType i s =
    case s of
        MonoType t -> show t
        Forall s0 -> "forall " ++ show (TVar i) ++ ". " ++ showPolyType (i+1) s0
