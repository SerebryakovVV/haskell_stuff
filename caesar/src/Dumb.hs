module Dumb
    ( someFunc, summ, square, amogus
    ) where

someFunc :: IO ()
someFunc = putStrLn "someFunc"


summ :: Int -> Int -> Int
summ x y = x + y


square :: Int -> Int
square x = x * x


amogus :: Int -> Int
amogus 0 = 100
amogus x = square $ summ x 10