module Dataflow where

import Exp

data Dataflow ctx where
    Start :: Dataflow ()
    Def :: Dataflow ctx -> Exp ctx a -> Dataflow (ctx, a)

size :: Num a => Dataflow ctx -> a
size df =
    case df of
        Start -> 0
        Def df0 _ -> 1 + size df0

infixl 7 #

(#) :: Dataflow ctx -> Exp ctx a -> Dataflow (ctx, a)
(#) = Def
