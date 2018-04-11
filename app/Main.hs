module Main where

import FinRec

ev = evalNetwork prelude
inp = input prelude
pp n = do
    putStrLn "=======================" 
    putStrLn (showNetwork n)
    putStrLn "" 

main :: IO ()
main = do
    let n = network
    pp (ev n)
    let n2 = inp (Number 99.0) 6 (inp (Number 1.0) 0 n)
    pp n2
    return ()

program :: Program
program = 
    Program "test" 
    [ Input
    , int 1
    , Apply "plus_i" [Var 0, Var 1]
    , Apply "plus_i" [Index 0, Index 1]
    , Apply "plus_i" [Index 0, Index 1]
    , Apply "plus_i" [Index 0, Index 1]
    , Input
    , Apply "plus_i" [Index 0, Index 1]
    , Apply "plus_i" [Index 0, Index 1]
    , Apply "times_i" [Index 0, Index 1]
    , Apply "float" [Var 8]
    , Apply "sqrt" [Index 0]
    , Apply "id" [Index 0]
    ]

network :: Network
network = initial $ program