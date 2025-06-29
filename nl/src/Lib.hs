module Lib
    ( nl
    ) where

import System.Environment


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


maybe