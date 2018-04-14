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
    , Apply "plus" [Var 0, Var 1]
    , Apply "plus" [Index 0, Index 1]
    , Apply "plus" [Index 0, Index 1]
    , Apply "plus" [Index 0, Index 1]
    , Input
    , Apply "plus" [Index 0, Index 1]
    , Apply "plus" [Index 0, Index 1]
    , Apply "times" [Index 0, Index 1]
    , Apply "sqrt" [Index 0]
    , Apply "id" [Index 0]
    ]
    Nothing

network :: Network
network = initial $ program