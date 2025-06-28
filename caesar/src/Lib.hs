-- module Lib
--   ( 
--   ) where

module Lib where
-- someFunc :: IO ()
-- someFunc = putStrLn "someFunc"

type Alphabet = [Char]

lowerAlphabet :: Alphabet
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: Alphabet
upperAlphabet = ['A' .. 'Z']

digits :: Alphabet
digits = ['0' .. '9']

isUpper :: Char -> Bool
isUpper x = x `elem` upperAlphabet 

isLower :: Char -> Bool
isLower x = x `elem` lowerAlphabet

isDigit :: Char -> Bool
isDigit x = x `elem` digits

-- point free style
-- isUpper = (`elem` upperAlphabet)
-- isLower = (`elem` lowerAlphabet)
-- isDigit = (`elem` digits)

isMisc :: Char -> Bool
-- isMisc x = not (isDigit x || isLower x || isUpper x) 
isMisc x = notElem x (digits ++ lowerAlphabet ++ upperAlphabet)


listLen :: [a] -> Int
listLen [] = 0
listLen (_ : xs) = 1 + listLen xs


indexOf :: Char -> Alphabet -> Maybe Int
indexOf ch [] = Nothing
indexOf ch (x : xs)
    | ch == x   = Just 0 
    | otherwise = case indexOf ch xs of
        Just n -> Just (n + 1)
        Nothing -> Nothing



elAt :: Int -> [a] -> Maybe a
elAt _ [] = Nothing
elAt 0 (x : _) = Just x
elAt i (_ : xs)
    | i > 0     = elAt (i - 1) xs
    | otherwise = Nothing


upperRot :: Int -> Char -> Char
upperRot num ch = 
    case indexOf ch upperAlphabet of
        Nothing -> error "Char is not in Upper Alphabet"
        Just i  -> 
            case elAt ((i + num) `mod` 26) upperAlphabet of
                Nothing -> error "Index out of bounds"
                Just e  -> e


lowerRot :: Int -> Char -> Char
lowerRot num ch = 
    case indexOf ch lowerAlphabet of
        Nothing -> error "Char is not in Lower Alphabet"
        Just i  -> 
            case elAt ((i + num) `mod` 26) lowerAlphabet of
                Nothing -> error "Index out of bounds"
                Just e  -> e


-- genRot :: Int -> Char -> Char
-- genRot num ch = 
--     case isUpper ch of
--         True  -> upperRot num ch
--         False -> case isLower ch of
--             True  -> lowerRot num ch
--             False -> case isDigit ch of
--                 True  -> case indexOf ch digits of
--                     Nothing -> error "The char is not in any alphabet"
--                     Just i  -> case elAt ((i + num) `mod` 10) digits of
--                         Nothing -> error "The char is not in any alphabet"
--                         Just t -> t
--                 False -> ch


genRot :: Int -> Char -> Char
genRot num ch
    | isUpper ch = upperRot num ch
    | isLower ch = lowerRot num ch
    | isDigit ch = case indexOf ch digits of
        Nothing -> error "The char is not in any alphabet"
        Just i  -> case elAt ((i + num) `mod` 10) digits of
            Nothing -> error "The char is not in any alphabet"
            Just t -> t
    | otherwise = ch

-- ghci> map (genRot 5) "abc=-=123"
-- "fgh=-=678"




transform :: (a -> b) -> [a] -> [b]
transform fun [] = []
transform fun (x : xs) = fun x : transform fun xs

cipher :: Int -> String -> String
cipher num str = transform (genRot num) str
-- cipher num str = map (genRot num) str




rot135 :: String -> String
rot135 str = map go str where
    go ch 
        | isUpper ch = upperRot 13 ch
        | isLower ch = lowerRot 13 ch
        | isDigit ch = case indexOf ch digits of
            Nothing -> error "The char is not in any alphabet"
            Just i  -> case elAt ((i + 5) `mod` 10) digits of
                Nothing -> error "The char is not in any alphabet"
                Just t -> t
        | otherwise = ch




countChar :: Char -> String -> Int
countChar _ []        = 0
countChar ch (x : xs) = 
    if ch == x 
    then 1 + countChar ch xs
    else 0 + countChar ch xs


countAllLowerChar :: String -> [Int]
countAllLowerChar str = map (\ch -> countChar ch str) lowerAlphabet