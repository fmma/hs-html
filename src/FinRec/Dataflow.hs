module FinRec.Dataflow where

import FinRec.Exp
import FinRec.Val

import Data.List ( intercalate )

type Node = (Maybe Val, Exp)

type Network = [Node]

type NetworkProcedure = Network

initial :: Program -> Network
initial (Program _ es) = zip (repeat Nothing) es

showNetwork :: Network -> String
showNetwork n = 
    let (vs, es) = unzip n
        showV i Nothing = replicate (2 - length (show i)) ' ' ++ show i ++ "| "
        showV i (Just v0) = showV i Nothing ++ show v0
        vs' = zipWith showV ([0..] :: [Int]) vs
        es' = map show es
        x = maximum (map length vs')
        showLine v e = v ++ replicate (x - length v) ' ' ++ " := " ++ e
    in intercalate "\n" (zipWith showLine vs' es')
