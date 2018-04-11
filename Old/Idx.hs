module Idx where

data Idx ctx a where
    Zero :: Idx (ctx, a) a
    Succ :: Idx ctx a -> Idx (ctx, b) a

idxToNum :: Num a => Idx ctx b -> a
idxToNum x =
    case x of
        Zero -> 0
        Succ x0 -> 1 + idxToNum x0

