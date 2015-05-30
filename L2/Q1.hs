-- 1

import Data.Char

binToInt :: String -> Int
binToInt str = sum $ zipWith toDec (reverse str) [0 .. (length str)]
	where toDec a b = (digitToInt a) * (2 ^ b)

get8firsts :: String -> Int -> String
get8firsts _ 8 = []
get8firsts (a:as) n = a:(get8firsts as (n+1))

toGray :: Int -> Int -> Int -> Int
toGray r g b = (r + g + b) `div` 3


toBin :: Int -> [Char]
toBin 0 = ['0']
toBin 1 = ['1']
toBin n
    | n `mod` 2 == 0 = toBin (n `div` 2) ++ ['0']
    | otherwise = toBin (n `div` 2) ++ ['1']


rgbToGray :: [String] -> [String]
rgbToGray [] = []
rgbToGray (a:as) = (toBin (toGray (binToInt (get8firsts a 0)) (binToInt (get8firsts (drop 8 a) 0)) (binToInt (get8firsts (drop 8 (drop 8 a)) 0)))):(rgbToGray as)

rgbToGs :: [[String]] -> [[String]]
rgbToGs img = map rgbToGray img

mediaLinha :: [String] -> Int
mediaLinha linha = (sum (map binToInt linha)) `div` length linha

mediaImg :: [[String]] -> Int
mediaImg img = (sum (map mediaLinha img)) `div` length img

checkLinha :: Int -> [String] -> [Bool]
checkLinha media [] = []
checkLinha media (a:as) = if ((binToInt a) <= media) then False:(checkLinha media as) else True:(checkLinha media as)

gsToPb :: [[String]] -> [[Bool]]
gsToPb img = map (checkLinha media) (rgbToGs img)
	where media = mediaImg (rgbToGs img)

{- 
Testes:
ex1 = [["000011110101010100001100", "111100001010101011110011"],["000011110111110100001100", "111111101010101011110011"]]
ex2 = [["001001010000000000000000", "000000000000000011011001"],["000000000000000000110010", "100000000000000001011110"]]
-> saida2: [[False,True],[False,True]]
-}