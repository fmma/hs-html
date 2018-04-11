module FinRec.Eval where

import FinRec.Context
import FinRec.Dataflow
import FinRec.Exp
import FinRec.Runtime
import FinRec.Val

evalExp :: Context -> [Val] -> Exp -> Val
evalExp ctx vs e =
    case e of
        Literal v -> v
        Apply op es -> lookupOperation op ctx (map (evalExp ctx vs) es)
        Index i -> index vs i
        Var x -> var vs x
        TupleExp es -> Tuple (map (evalExp ctx vs) es)
        Project e0 i -> project i (evalExp ctx vs e0)

eval :: Context -> [Val] -> [Exp] -> [Val]
eval ctx vs es =
    case es of
        [] -> []
        (e:es0) ->
            let v = evalExp ctx vs e
            in v : eval ctx (v:vs) es0

evalNetwork :: Context -> Network -> Network
evalNetwork ctx n = evalZip ctx ([], n)

input :: Context -> Val -> Int -> Network -> Network
input ctx v i n = evalZip ctx (zipStart v i n)

inputAll :: [Val] -> Network -> Network
inputAll vs n =
    case vs of
        [] -> n
        v : vs0 ->
            case n of
                [] -> runtimeError "not enough input for values"
                (_, Input) : n0 -> (Just v, Input) : inputAll vs0 n0
                (v0, e) : n0 -> (v0, e) : inputAll vs n0
networkResultVal :: Network -> Val
networkResultVal n = 
    case fst (last n) of
        Just v -> v
        Nothing -> runtimeError "networkResultVal"

evalProgram :: Context -> Program -> [Val] -> Val
evalProgram ctx p vs = networkResultVal (evalNetwork ctx (inputAll vs (initial p)))

evalProgramList :: Context -> [Program] -> Context
evalProgramList ctx ps = 
    case ps of
        [] -> ctx
        p:ps0 -> evalProgramList ((programName p, evalProgram ctx p) : ctx) ps0

type Zip a = ([a], [a])

zipStart :: Val -> Int -> Network -> Zip Node
zipStart v 0 ((_, Input):ns) = ([], (Just v, Input):ns)
zipStart v i (n:ns) | i > 0 =
    let (ns0, ns1) = zipStart v (i-1) ns
    in (ns0 ++ [n], ns1)
zipStart _ _ _ = runtimeError "zipStart"

evalZip :: Context -> Zip Node -> Network
evalZip ctx (ns0, ns1) =
    case ns1 of
        [] -> reverse ns0
        (n:ns1') -> evalZip ctx (evalZipExp ctx n (map fst ns0) : ns0, ns1')

evalZipExp :: Context -> Node -> [Maybe Val] -> Node
evalZipExp _ (v, Input) _ = (v, Input)
evalZipExp ctx (_, e) vs = (evalExpM ctx vs e, e)

evalExpM :: Applicative m => Context -> [m Val] -> Exp -> m Val 
evalExpM ctx vs e =
    case e of
        Literal v -> pure v
        Apply op es -> lookupOperation op ctx <$> traverse (evalExpM ctx vs) es
        Index i -> index vs i
        Var i -> var vs i
        TupleExp es -> Tuple <$> traverse (evalExpM ctx vs) es
        Project e0 i -> project i <$> evalExpM ctx vs e0
