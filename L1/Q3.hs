-- 3

import Data.Char

    -- Auxiliares
charToInt :: Char -> Int
charToInt a
    | up >= 'A' && up <= 'I' = (ord up) - (ord 'A') + 1
    | up >= 'J' && up <= 'R' = (ord up) - (ord 'J') + 1
    | otherwise = (ord up) - (ord 'S') + 1
    where up = if (a >= 'a' && a <= 'z') then chr ((ord a) + (ord 'A' - ord 'a')) else a

numCharToInt :: Char -> Int
numCharToInt a = (ord a) - (ord '0')

stringToNum :: [Char] -> [Int]
stringToNum word = map charToInt word

numStringToInt :: [Char] -> [Int]
numStringToInt num = map numCharToInt num

keepSum :: [Int] -> Int
keepSum (a:[]) = a
keepSum as = keepSum (numStringToInt (show (sum as)))
    --

pitagoras :: [Char] -> Int
pitagoras word = keepSum (stringToNum word)