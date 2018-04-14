module FinRec.TypeInference where

import FinRec.Context
import FinRec.Exp
import FinRec.Runtime
import FinRec.Type
import FinRec.PolyType
import FinRec.Val

import Data.List ( nub )

inst :: PolyType -> Int -> (Type, Int)
inst s i = (shiftVars i (monotype s), i + polyArity s)

inferExps :: PolyTypeContext -> [Type] -> [Exp] -> Int -> Either String ([Type], Int, [(Type, Type)])
inferExps ctx ts es i =
    case es of
        [] -> Right ([], i, [])
        (e:es0) -> do
            (t0, i', g) <- inferExp ctx ts e i
            (ts0, i'', g') <- inferExps ctx ts es0 i'
            return (t0: ts0, i'', g ++ g')

inferExp :: PolyTypeContext -> [Type] -> Exp -> Int -> Either String (Type, Int, [(Type, Type)])
inferExp ctx ts e i =
    case e of
        Literal v -> Right (typeof v, i, [])
        Apply op es -> do
            s <- maybe (Left $ "unbound function " ++ op) Right (lookup op ctx)
            let (t0, i') = inst s i
            (ts0, i'', g) <- inferExps ctx ts es i'
            let t' = TVar i''
            return (t', i'' + 1, (t0, funType ts0 t') : g)
        Index i0 ->
            if i0 < length ts
                then return (ts !! i0, i, [])
                else Left $ "undbound index" ++ show (Index i)
        Var x -> 
            if x < length ts
                then return (reverse ts !! x, i, [])
                else Left $ "unbound variable " ++ show (Var x)
        TupleExp es -> 
            (\ (ts0, i', g) -> (tupleType ts0, i', g)) <$> inferExps ctx ts es i
        Project e0 n -> do
            (t0, i', g) <- inferExp ctx ts e0 i
            let t' = TVar <$> [i' .. i' + n + 1]
            return (last (init t'), i' + n + 2, (t0, openTupleType t'):g)
        Input -> Right (TVar i, i + 1, [])

inferProgram :: PolyTypeContext -> Program -> Either String [Type]
inferProgram ctx (Program _ es _) = do
    (ts, _, g) <- infer ctx [] es 0
    g' <- unify g
    let g'' = map ( \(TVar i, t) -> (i, t)) g'
    return (map (substituteTypeVar'' g'') ts)

inferProgramList :: PolyTypeContext -> [Program] -> Either String PolyTypeContext
inferProgramList ctx ps =
    case ps of
        [] -> Right ctx
        (p : ps0 ) -> do
            ts <- errorIn p $ inferProgram ctx p
            let t = (programName p, makeProgramCtxPolyType (zip (programBody p) ts))
            inferProgramList (t : ctx) ps0

makeProgramCtxPolyType :: [(Exp, Type)] -> PolyType
makeProgramCtxPolyType p =
    let inTypes = map snd $ filter (\ (e, _) -> case e of Input -> True; _ -> False) p
        outType = snd (last p) 
    in quantifyType $ funType inTypes outType

infer :: PolyTypeContext -> [Type] -> [Exp] -> Int -> Either String ([Type], Int, [(Type, Type)])
infer ctx ts es i =
    case es of
        [] -> Right ([], i, [])
        (e:es0) ->
            do (t, i', g') <- errorIn e $ inferExp ctx ts e i
               (ts0, i'', g'') <- infer ctx (t : ts) es0 i'
               return (t : ts0, i'', g' ++ g'')

unifyOpenTuple :: Int -> Type -> [Type] -> [(Type, Type)]
unifyOpenTuple j t ts =
    case (t, ts) of
        (TCon "NIL" [], []) -> []
        (TVar i, []) -> [(TVar i, TCon "NIL" [])]
        (TVar i, t0:ts0) -> (TVar i, TCon "::" [t0, TVar j]) : unifyOpenTuple (j+1) (TVar j) ts0
        (TCon "::" [t0, t1], t0':ts0) -> (t0, t0'): unifyOpenTuple j t1 ts0
        _ -> runtimeError $ "unifyOpenTuple " ++ show t ++ " " ++ show ts

unify :: [(Type, Type)] -> Either String [(Type, Type)]
unify g =
    let j = foldl max 0 $ freeTypeVars' g
        unifyCons f0 ts0 f1 ts1 g0
            | f0 == f1 && length ts0 == length ts1 
            = unify $ zip ts0 ts1 ++ g0
            | f0 == "::" && f1 == "()"
            = unify $ (unifyOpenTuple (1 + j) (TCon f0 ts0) ts1) ++ g0
            | f0 == "()" && f1 == "::"
            = unify $ unifyOpenTuple (1 + j) (TCon f1 ts1) ts0 ++ g0
            | otherwise
            = Left $ "Type mismatch in " ++ show (TCon f0 ts0) ++ " = " ++ show (TCon f1 ts1)  
        unifyVar i t g0 
            | i `elem` freeTypeVars' g0 && i `notElem` freeTypeVars t
            = unify $ (TVar i, t) : substituteTypeVar' i t g0
            | i `elem` freeTypeVars t
            = Left $ "Occurs check failed in " ++ show (TVar i) ++ " = " ++ show t
            | otherwise
            = ((TVar i, t):) <$> unify g0
    in
    case g of
        [] -> Right []
        (t0, t1):g0 
            | t0 == t1 -> unify g0
            | otherwise ->
                case (t0, t1) of
                    (TCon f0 ts0, TCon f1 ts1) -> unifyCons f0 ts0 f1 ts1 g0
                    (TCon _ _, TVar _) -> unify $ (t1, t0):g0
                    (TVar i, _) -> unifyVar i t1 g0
{-
unify ((t0, t1):ts) | t0 == t1 = unify ts
unify ((t0, t1):ts) =
    let j = foldl max 0 $ freeTypeVars' ((t0, t1):ts)
    in
    case (t0, t1) of
        (TCon f0 ts0, TCon f1 ts1) 
            | f0 == f1 && length ts0 == length ts1 
            -> unify $ zip ts0 ts1 ++ ts
            | f0 == "::" && f1 == "()"
            -> unify $ unifyOpenTuple (1 + j) t0 ts1 ++ ts
            | f0 == "()" && f1 == "::"
            -> unify $ unifyOpenTuple (1 + j) t1 ts0 ++ ts 
            | otherwise 
            -> Nothing
        (TCon f ts0, TVar i) -> unify $ (TVar i, TCon f ts0):ts
        (TVar i, _)
            | i `elem` freeTypeVars' ts && i `notElem` freeTypeVars t1
            -> unify $ (TVar i, t1) : substituteTypeVar' i t1 ts
            | i `elem` freeTypeVars t1
            -> Nothing -- "Occurs check failed"
            | otherwise
            -> ((TVar i, t1):) <$> unify ts
unify [] = Right []
-}

freeTypeVars' :: [(Type, Type)] -> [Int]
freeTypeVars' ts = nub $ concatMap (\ (t0, t1) -> freeTypeVars t0 ++ freeTypeVars t1) ts

substituteTypeVar' :: Int -> Type -> [(Type, Type)] -> [(Type, Type)]
substituteTypeVar' i t ts = map (\ (t0, t1) -> (substituteTypeVar i t t0, substituteTypeVar i t t1)) ts

substituteTypeVar'' :: [(Int, Type)] -> Type -> Type
substituteTypeVar'' g t =
    case g of
        [] -> t
        (i, t0) : g0 -> substituteTypeVar'' g0 (substituteTypeVar i t0 t)

