module Example where

import Fun
import Exp
import Dataflow

example :: Dataflow ((((((), Int), Int), Int), Int), Bool)
example = 
    Start 
    # constant (5 :: Int)
    # constant (4 :: Int)
    # App Plus (PP x0 x1)
    # App Plus (PP x0 x2)
    # App Equals (PP x0 x1)
