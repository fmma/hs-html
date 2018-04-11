module FinRec.TypeCheck where

import FinRec.Context
import FinRec.Exp
import FinRec.Runtime
import FinRec.Type
import FinRec.Val

typecheckExp :: TypeContext -> [Type] -> Exp -> Maybe Type
typecheckExp ctx ts e =
    case e of
        Literal v -> Just (typeof v)
        Apply op es -> do
            (ts0, t) <- lookup op ctx >>= matchFunType
            ts1 <- mapM (typecheckExp ctx ts) es
            if ts0 == ts1
                then Just t
                else Nothing
        Index i -> 
            if i < length ts
                then Just (ts !! i)
                else Nothing
        Var x ->
            if x < length ts
                then Just (reverse ts !! x)
                else Nothing
        TupleExp es -> 
            tupleType <$> mapM (typecheckExp ctx ts) es
        Project e0 i -> do
            t0 <- typecheckExp ctx ts e0
            ts0 <- matchTupleType t0
            if i >= 0 && i < length ts0
                then Just (ts0 !! i)
                else Nothing 
        Input -> runtimeError "Cannot typecheck polymorhpic input. Use type inference instead."

typecheckProgram :: TypeContext -> Program -> Maybe [Type]
typecheckProgram ctx (Program _ es) = typecheck ctx [] es

typecheckProgramList :: TypeContext -> [Program] -> Maybe TypeContext
typecheckProgramList ctx ps =
    case ps of
        [] -> Just ctx
        (p : ps0 ) -> do
            ts <- typecheckProgram ctx p
            let t = (programName p, makeProgramCtxType (zip (programBody p) ts))
            typecheckProgramList (t : ctx) ps0

makeProgramCtxType :: [(Exp, Type)] -> Type
makeProgramCtxType p =
    let inTypes = map snd $ filter (\ (e, _) -> case e of Input -> True; _ -> False) p
        outType = snd (last p) 
    in funType inTypes outType

typecheck :: TypeContext -> [Type] -> [Exp] -> Maybe [Type]
typecheck ctx ts es =
    case es of
        [] -> Just []
        (e:es0) ->
            do t <- typecheckExp ctx ts e
               ts0 <- typecheck ctx (t : ts) es0
               return (t : ts0)
