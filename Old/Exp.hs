module Exp where

import Idx
import Fun

data Exp ctx a where
    Var :: Idx ctx a -> Exp ctx a
    App :: Fun a b -> Exp ctx a -> Exp ctx b
    PP :: Exp ctx a -> Exp ctx b -> Exp ctx (a, b)
    Unit :: Exp ctx ()

constant :: Show b => b -> Exp ctx b
constant x = App (Constant x) Unit

x0 :: Exp (ctx, a) a
x0 = Var Zero

x1 :: Exp ((ctx, a), b) a
x1 = Var (Succ Zero)

x2 :: Exp (((ctx, a), b1), b2) a
x2 = Var (Succ (Succ Zero))

x3 :: Exp ((((ctx, a), b1), b2), b3) a
x3 = Var (Succ (Succ (Succ Zero)))