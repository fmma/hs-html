module FinRec.Desugar where

import FinRec.Exp

desugar :: Program -> Program
desugar = implicitInput

implicitInput :: Program -> Program
implicitInput (Program n es fi) = Program n ((const Input <$> [0 .. underflow 0 es - 1]) ++ es) fi

underflow :: Int -> [Exp] -> Int
underflow n es =
    case es of
        [] -> 0
        e:es0 -> (maxIndex e - n) `max` underflow (n + 1) es0

maxIndex :: Exp -> Int
maxIndex e =
    case e of
        Literal _ -> 0
        Apply _ es -> foldl (\ i e0 -> i `max` maxIndex e0) 0 es
        Index i -> i + 1
        Var _ -> 0
        TupleExp es -> foldl (\ i e0 -> i `max` maxIndex e0) 0 es
        Project e0 _ -> maxIndex e0
        Input -> 0