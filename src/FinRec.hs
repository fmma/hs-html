module FinRec
( module FinRec.Context
, module FinRec.Exp
, module FinRec.Eval
, module FinRec.Dataflow
, module FinRec.DefaultContext
, module FinRec.Desugar
, module FinRec.Parser
, module FinRec.PolyType
, module FinRec.Type
, module FinRec.TypeCheck
, module FinRec.TypeInference
, module FinRec.Val
, prelude, preludeTypes, types, check
) where

import FinRec.Context
import FinRec.Exp
import FinRec.Eval
import FinRec.Dataflow
import FinRec.DefaultContext
import FinRec.Desugar
import FinRec.Parser
import FinRec.PolyType
import FinRec.Type
import FinRec.TypeCheck
import FinRec.TypeInference
import FinRec.Val

import System.IO.Unsafe ( unsafePerformIO )

prelude :: Context
prelude = fst $ unsafePerformIO readPrelude

preludeTypes :: PolyTypeContext
preludeTypes = snd $ unsafePerformIO readPrelude

inferProgramListIO :: PolyTypeContext -> [Program] -> IO PolyTypeContext
inferProgramListIO ctx ps =
    case inferProgramList ctx ps of
        Left err -> do 
            putStrLn err
            return []
        Right res -> return res

readPrelude :: IO (Context, PolyTypeContext)
readPrelude = do
    ps <- map desugar . parse infixTable <$> readFile "src\\FinRec\\prelude.df"
    ts <- inferProgramListIO typeContext ps
    let ctx = evalProgramList valueContext ps
    return $ (ctx, ts)

types :: IO ()
types = do
    ps <- map desugar . parse infixTable <$> readFile "src\\FinRec\\prelude.df"
    ctx <- inferProgramListIO typeContext ps
    mapM_ (putStrLn . printProgramInfo) (reverse ctx)

printProgramInfo :: (String, PolyType) -> String
printProgramInfo (n, t) = n ++ ": " ++ show t

check :: String -> IO ()
check s = case inferProgramList preludeTypes . parse infixTable $ s of
    Right ctx -> putStrLn $ show $ head ctx
    Left err -> putStrLn err
