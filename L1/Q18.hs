-- 18

    -- Auxiliares p/ 8-rainhas
        -- Testa se a nova peca esta na diagonal de outra
checkDiagonal :: Int -> [(Int, Int)] -> Bool
checkDiagonal _ [] = False -- Caso base (não influencia o 'OR')
checkDiagonal pos ((deltaLinha, col):xs) = (abs (pos - col) == deltaLinha) || checkDiagonal pos xs

       -- Checa se a nova peca esta na mesma coluna que outra
checkColuna :: Int -> [Int] -> Bool
checkColuna pos curr = elem pos curr

        -- Testa se o movimento eh valido
test :: Int -> [Int] -> Bool
test pos curr = not (checkColuna pos curr || checkDiagonal pos tab)
    where tab = zip [1..] curr  -- (deltaLinha = dist. da linha da peça que se quer jogar p/ linha de uma peça na solução atual. 
                                -- Lista com tuplas na forma (deltaLinha, coluna)
                                -- [Note que o as posições estão indo da linha mais abaixo no tabuleiro p/ mais acima]
    
        -- Calcula todas as solucoes de um tabuleiro n x n
allSolutions :: Int -> [[Int]]
allSolutions n = result
    where solve 0 = [[]]
          solve k = [q:qs | qs <- solve (k-1), q <- [1..n], test q qs]
          result = map reverse (solve n) -- Precisa reverter pra ficar na ordem certa, pq a gente vai inserindo no 'topo', como se fosse uma pilha

        -- Todos os resultados (melhora o desempenho pq n precisa ficar recalculando toda vez que chamar arrumarMesas)
solutions :: [[Int]]
solutions = allSolutions 8
    --

arrumarMesas :: Int -> [Int]
arrumarMesas n = result
    where k = minimum [n, 92]
          result = (head . drop (k-1)) solutions
