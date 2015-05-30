-- 19 

    -- Testa se uma lista soh contem eltos distintos
checkDistinct :: [Int] -> [Int] -> Bool
checkDistinct _ [] = True
checkDistinct xs (a:as) | a `elem` xs = False
                        | otherwise   = checkDistinct (xs++[a]) as

   -- Generaliza o teste pra uma matrix
check :: [[Int]] -> Bool
check as = foldr (&&) True (map (checkDistinct []) as)

   -- Retorna transposta da matrix de entrada
getColunas :: Int -> [[Int]] -> [[Int]]
getColunas 10 _ = []
getColunas i as = tmp:(getColunas (i+1) as)
    where magic = (head.(drop (i-1)))                -- Funcao que vai pegar o numero na coluna desejada
          tmp = map magic as

   -- Aproveitando a funcao da Q5...
   -- Retorna uma submatrix com 'centro' em (i, j) e tamanho nxn (vai ser usada no teste da 3a regra)
getQuadro :: Int -> Int -> Int -> [[Int]] -> [Int]
getQuadro i j n xs = viz
    where k = (n-1) `div` 2 -- Qntd de numeros antes e depois do 'centro' da vizinhanca
          iniL = i - k 
          fimL = i + k
          iniC = j - k
          fimC = j + k
          tmp = zip [1..(length xs)] xs                                                   -- (i, [valores na i-esima linha])
          mtx = [(li, co, val) | (li, as) <- tmp, (co, val) <- (zip [1..(length xs)] as)] -- (i, j, valor na linha i e coluna j) 
          test (x, y, v) = (x >= iniL) && (x <= fimL) && (y >= iniC) && (y <= fimC)       -- testa se o numero esta na vizinhanca
          viz = [v | (x, y, v) <- mtx, test (x, y, v)]

checkColunas :: [[Int]] -> Bool
checkColunas as = check (getColunas 1 as)

checkQuadros :: [[Int]] -> Bool
checkQuadros as = check tmp
    where tmp = [getQuadro 2 2 3 as]++[getQuadro 2 5 3 as]++[getQuadro 2 8 3 as]++[getQuadro 5 2 3 as]++[getQuadro 5 5 3 as]++[getQuadro 5 8 3 as]++[getQuadro 8 2 3 as]++[getQuadro 8 5 3 as]++[getQuadro 8 8 3 as] 

sudoku :: [[Int]] -> Bool
sudoku as = (check as) && (checkColunas as) && (checkQuadros as)

-- Exemplo funcionando 
--ex = sudoku [[5, 3, 4, 6, 7, 8, 9, 1, 2], [6, 7, 2, 1, 9, 5, 3, 4, 8], [1, 9, 8, 3, 4, 2, 5, 6, 7], [8, 5, 9, 7, 6, 1, 4, 2, 3], [4, 2, 6, 8, 5, 3, 7, 9, 1], [7, 1, 3, 9, 2, 4, 8, 5, 6], [9, 6, 1, 5, 3, 7, 2, 8, 4], [2, 8, 7, 4, 1, 9, 6, 3, 5], [3, 4, 5, 2, 8, 6, 1, 7, 9]]