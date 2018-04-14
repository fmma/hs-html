module FinRec.Infix where

import FinRec.Runtime

import Data.List ( sort, delete )

data Fixity
    = Infixl
    | Infixr
    | Infix
    deriving (Eq, Ord, Show)

type InfixTable = [(Int, (Fixity, [String]))]

updateInfixTable :: String -> Fixity -> Int -> InfixTable -> InfixTable
updateInfixTable op f p t =
    case lookup p t of
        Nothing -> sort $ (p, (f, [op])):t 
        Just (f0, ops)
            | f0 == f 
            -> sort $ (p, (f0, op:ops)) : delete (p, (f0, ops)) t
        _ -> runtimeError $ "Bad fixity for binop(" ++ show (op, f, p) ++ "). Precedence level " ++ show p ++ " is already defined as " ++ show f ++ "."
