module Lib
    ( main1, main2, f
    ) where

data QQ a = QQ1 (a -> a)
          | QQ2 a a

main1 :: IO ()
main1 = putStrLn "someFunc"

main2 :: IO ()
main2 = main1

f :: Num a => QQ a -> a
f x =
    case x of
        QQ1 g -> g 0
        QQ2 a b -> qq a b

qq :: Num a => a -> a -> a
qq a b = a + b
