-- 5

mostBeautifulChain :: [String] -> Int
mostBeautifulChain chains = sum  (map getValue (getString ( concatenate (organize chains))))

getString :: String -> String
getString [] = []
getString (a:as) | a/='.' = getString as
				 | otherwise = takeWhile (/='.') (dropWhile (=='.') (a:as))

concatenate :: [String] -> String
concatenate [] = []
concatenate (a:as) = a ++ concatenate as

organize :: [String] -> [String]
organize chains = [left] ++ mids ++ [right]
	where 
		mids = midChains chains
		bestBorderLeft = bestChain "." $ borderLeftChains chains
		bestBorderRight = bestChain "." $ borderRightChains chains
		bestRight = bestChain "." (rightChains chains)
		bestLeft = bestChain "." (leftChains chains)
		left = if (getBeauty bestBorderRight) > (getBeauty bestLeft) then bestBorderRight else bestLeft
		right = if (getBeauty bestBorderLeft) > (getBeauty bestRight) then bestBorderLeft else bestRight

		--rest = remove bestLeft (remove bestRight (remove bestMid chains))

remove :: String -> [String] -> [String]
remove best chains  = aux best [] chains
	where 
		aux _ before [] = before
		aux temp before (a:as) = if temp == a then (before ++ as) else (aux temp (before ++ [a]) as)

isBorderLeft :: String -> Bool
isBorderLeft (a:b:c:cs) = if (a/='.' && b=='.' && c/='.' && a>b) then True else False

isBorderRight :: String -> Bool
isBorderRight (a:b:c:cs) = if (a/='.' && b=='.' && c/='.' && a<=b) then True else False

borderLeftChains :: [String] -> [String]
borderLeftChains chains = filter isBorderLeft chains

borderRightChains :: [String] -> [String]
borderRightChains chains = filter isBorderRight chains

isMid :: String -> Bool
isMid (a:b:c:cs) = if (a/='.' && b/='.' && c/='.') then True else False

midChains :: [String] -> [String]
midChains chains = filter isMid chains

isRight :: String -> Bool
isRight (a:b:c:cs) = if c=='.' then True else False

rightChains :: [String] -> [String]
rightChains chains = filter isRight chains

isLeft :: String -> Bool
isLeft (c:cs) = if c=='.' then True else False

leftChains :: [String] -> [String]
leftChains chains = filter isLeft chains

bestChain :: String -> [String] ->  String
bestChain chain []  = chain
bestChain chain (c:cs) | (getBeauty c) >= (getBeauty chain) = bestChain c cs
                       | otherwise = bestChain chain cs

getBeauty :: String -> Int
getBeauty chain = sum (map getValue chain)

getValue :: Char -> Int
getValue '.' = 0
getValue '0' = 0
getValue '1' = 1
getValue '2' = 2
getValue '3' = 3
getValue '4' = 4
getValue '5' = 5
getValue '6' = 6
getValue '7' = 7
getValue '8' = 8
getValue '9' = 9