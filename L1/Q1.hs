-- 1

pascal :: Int -> Int -> Int
pascal _ 0 = 1
pascal m n 
    | m == n = 1
    | otherwise = (pascal (m-1) (n-1)) + (pascal (m-1) n)