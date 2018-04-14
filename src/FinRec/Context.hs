module FinRec.Context where

import FinRec.Runtime
import FinRec.PolyType
import FinRec.Type
import FinRec.Val

type Context = [(String, [Val] -> Val)]

type TypeContext = [(String, Type)]

type PolyTypeContext = [(String, PolyType)]

lookupOperation :: String -> [(String, a)] -> a
lookupOperation a b =
    case lookup a b of
        Just x -> x
        Nothing -> runtimeError "undefined operation"

variables :: Context -> [String]
variables = map fst
