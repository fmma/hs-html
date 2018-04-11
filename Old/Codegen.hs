module Codegen where

import Idx
import Fun
import Exp
import Dataflow

var :: Int -> String
var x = "ctx[i - " ++ show x ++ "]" 

class Codegen a where
    codegen :: a -> [String]
    codegen x = [codegen' x]
    codegen' :: a -> String
    codegen' a = unlines (codegen a)

instance Codegen (Idx ctx a) where
    codegen' x = var (idxToNum x)

instance Codegen (Fun a b) where
    codegen' f =
        case f of
            Constant x -> "(() => " ++ show x ++ ")"
            Fst -> "(x => x[0])"
            Snd -> "(x => x[1])"
            Plus -> "(x => x[0] + x[1])"
            TernCond -> "(x => x[0] ? x[1] : x[2])"
            Equals -> "(x => x[0] === x[1])"

instance Codegen (Exp ctx a) where
    codegen e =
        case e of
            Var idx -> codegen idx
            App f e0 -> codegen f ++ ["("] ++ codegen e0 ++ [")"]
            PP e0 e1 -> ["["] ++ codegen e0 ++ [","] ++ codegen e1 ++ ["]"]
            Unit -> ["[]"]
            
instance Codegen (Dataflow ctx) where
    codegen df = 
        ["const ctx = [];"] ++
        ["const code ="] ++ 
        codegenDataflow df ++
        ["];"]
            
codegenDataflow :: Dataflow ctx -> [String]
codegenDataflow df =
    case df of
        Start -> []
        Def Start e -> ["[ () => "] ++ codegen e
        Def df0 e -> codegenDataflow df0 ++ [", () => "] ++ codegen e
