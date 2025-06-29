import Data.Char

main :: IO ()
main = do 
    line <- getLine
    putStr line


interactiveLines :: Int -> IO ()
interactiveLines counter = do
    line <- getLine
    if null line
        then return ()
        else do
            putStrLn (show counter ++ ") " ++ map toUpper line)
            interactiveLines (counter + 1)
    