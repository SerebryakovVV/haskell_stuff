module Lib
    ( nl, numberAllLines, parseArguments, fromMb
    ) where

import System.Environment




type NumberedLine = (Maybe Int, String)
type NumberedLines = [NumberedLine]



numberAllLines :: [String] -> NumberedLines
numberAllLines lines = 
    let go :: Int -> [String] -> NumberedLines
        go _ [] = []
        go counter (x : xs) = (Just counter, x) : go (counter + 1) xs   
     in go 1 lines 



nl :: IO ()
nl = do 
    args <- getArgs
    case parseArguments args of
        Nothing -> putStrLn "Number of args is not equal to one"
        Just p -> putStrLn p


parseArguments :: [String] -> Maybe FilePath
parseArguments [path] = Just path
parseArguments _ =  Nothing


fromMb :: a -> Maybe a -> a
fromMb _ (Just v) = v
fromMb v Nothing  = v






