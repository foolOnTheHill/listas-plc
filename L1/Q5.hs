-- 5

    -- Verifica se existe a vizinhanca
wellDefined :: Int -> Int -> Int -> Int -> Bool
wellDefined x y n dim = (x > k) && (x <= l) && (y > k) && (y <= l)
    where k = (n-1) `div` 2
          l = dim - k

    -- Como o tamanho da vizinhanca, n,  deve ser ímpar, n*n também é impar e sempre existe um elto no 'meio' da lista
mediana :: [Int] -> Int
mediana as = r
    where l = length as
          k = (l-1) `div` 2
          r = head (drop k as)

getVizinhanca :: Int -> Int -> Int -> [[Int]] -> [Int]
getVizinhanca i j n xs = viz
    where k = (n-1) `div` 2 -- Qntd de numeros antes e depois do 'centro' da vizinhanca
          iniL = i - k 
          fimL = i + k
          iniC = j - k
          fimC = j + k
          tmp = zip [1..(length xs)] xs                                                   -- (i, [valores na i-esima linha])
          mtx = [(li, co, val) | (li, as) <- tmp, (co, val) <- (zip [1..(length xs)] as)] -- (i, j, valor na linha i e coluna j) 
          test (x, y, v) = (x >= iniL) && (x <= fimL) && (y >= iniC) && (y <= fimC)       -- testa se o numero esta na vizinhanca
          viz = [v | (x, y, v) <- mtx, test (x, y, v)]

    -- Quicksort
qs :: [Int] -> [Int]
qs [] = []
qs (a:as) = (qs [x | x <- as, x <= a])++[a]++(qs [x | x <- as, x > a])

    -- Aplica o filtro em cada linha da matriz
applyFilter :: Int -> [(Int, Int)] -> [[Int]] -> [Int]
applyFilter _ [] _ = []
applyFilter n ((i, j):xs) mtx = if (wellDefined i j n (length mtx))
                                then (mediana (qs (getVizinhanca i j n mtx))):(applyFilter n xs mtx)
                                else 0:(applyFilter n xs mtx)

filtroMediana :: [[Int]] -> Int -> [[Int]]
filtroMediana as n = aux 1 n as
    where aux curr n mtx | curr < (length mtx)  = (applyFilter n [(curr, y) | y <- [1..(length mtx)]] mtx):(aux (curr+1) n mtx)
                         | curr == (length mtx) = [applyFilter n [(curr, y) | y <- [1..(length mtx)]] mtx]

-- filtroMediana [[9, 4, 5, 0, 8], [10, 3, 2, 1, 7], [9, 1, 6, 3, 15], [0, 3, 8, 10, 1], [1, 16, 9, 12, 7]]