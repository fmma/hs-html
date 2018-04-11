module FinRec
( module FinRec.Context
, module FinRec.Exp
, module FinRec.Eval
, module FinRec.Dataflow
, module FinRec.DefaultContext
, module FinRec.Parser
, module FinRec.Type
, module FinRec.TypeCheck
, module FinRec.TypeInference
, module FinRec.Val
, prelude, preludeTypes, preludeTypeCheck
) where

import FinRec.Context
import FinRec.Exp
import FinRec.Eval
import FinRec.Dataflow
import FinRec.DefaultContext
import FinRec.Parser
import FinRec.Type
import FinRec.TypeCheck
import FinRec.TypeInference
import FinRec.Val

import System.IO.Unsafe ( unsafePerformIO )

prelude :: Context
prelude = fst $ unsafePerformIO readPrelude

preludeTypes :: PolyTypeContext
preludeTypes = snd $ unsafePerformIO readPrelude

readPrelude :: IO (Context, PolyTypeContext)
readPrelude = do
    ps <- parse <$> readFile "src\\FinRec\\prelude.df"
    let Just ts = inferProgramList typeContext ps
    let ctx = evalProgramList valueContext ps
    return $ (ctx, ts)

preludeTypeCheck :: IO ()
preludeTypeCheck = do
    ps <- parse <$> readFile "src\\FinRec\\prelude.df"
    let Just ctx = inferProgramList typeContext ps
    mapM_ (putStrLn . printProgramInfo) (reverse ctx)

printProgramInfo :: (String, PolyType) -> String
printProgramInfo (n, t) = n ++ ": " ++ show t 