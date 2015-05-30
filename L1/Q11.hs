-- 11

testeListas :: [Int] -> [Int]
testeListas as = [y | (x, y) <- xs, x == 1 || x == 2 || y > (head (drop (x-3) as))]
    where xs = (zip [1..] as)  -- Pra pegar os indices dos valores