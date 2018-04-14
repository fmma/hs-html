module FinRec.Runtime where

index :: [a] -> Int -> a
index (v : _) 0 = v
index (_ : vs) i | i > 0 = index vs (i-1)
index _ _ = runtimeError "index out of bounds"

var :: [a] -> Int -> a
var vs i = index (reverse vs) i
        
runtimeError :: String -> a
runtimeError mes = error ("Runtime error: " ++ mes)

errorIn :: Show a => a -> Either String b -> Either String b
errorIn x (Left y) = Left $ "In " ++ show x ++ "\n" ++ y
errorIn _ (Right y) = Right y
