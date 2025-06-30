-- use stack ghci

-- page 78
-- TODO: The functions lines , unlines , words , and unwords are very helpful utilities when
-- working with strings. Implement these functions yourself using recursive definitions!
-- Remember that Strings are just lists of chars, so you can pattern match them and can
-- create definitions with more parameters inside your functions using a let binding. Use
-- the pattern matches with guards to construct these functions.


module Main (main) where

import System.Environment

import Lib

printHelpText :: String -> IO ()
printHelpText msg = do
    putStrLn (msg ++ "\n")
    progName <- getProgName
    putStrLn ("Usage: " ++ progName ++ " <filename>")



-- main :: IO ()
-- main = do
--     cliArgs <- getArgs
--     let mFilePath = parseArguments cliArgs
--     maybe
--       (printHelpText "Missing filename")
--       (\filePath -> putStrLn filePath)
--       mFilePath

main :: IO ()
main = do
    linesList <- readLines "C:/Users/sereb/main_files/coding/haskell_stuff/trash.txt"
    print $ numberAllLines linesList


-- "C:/Users/sereb/main_files/coding/haskell_stuff/trash.txt"
readLines :: FilePath -> IO [String]
readLines filePath = do
    contents <- readFile filePath
    return (lines contents)


