-- 6

count :: Int -> Char -> Int -> [Char] -> [Char] -> [Char]
count 0 _ _ _ str = str
count n '$' _ next _ = count (n-1) (head next) 0 "" (next ++ (if n > 1 then "$" else ""))
count n rep curr next (a:as)
    | a == rep = count n rep (curr+1) next as
    | otherwise = count n a 0 (next ++ (show curr) ++ [rep]) (a:as) 

looknsay :: Int -> [Char]
looknsay 0 = "1"
looknsay n = count n '1' 0 "" "1$"