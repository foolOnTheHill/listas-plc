-- 14

    -- Cadeia de entrada => [Int]
    -- Tabela de transicoes => [(Int, Int, Int)] ~ Equivalente à delta: Estados x {0, 1} -> Estados
    -- Estados de aceitação => [Int]
    -- Estado inicial => Int

    -- Funcao de transicao: determina o proximo estado
trans :: Int -> Int -> [(Int, Int, Int)] -> Int
trans qA ch ((qP, entr, nxt):as) | qA == qP && ch == entr = nxt
                                 | otherwise = trans qA ch as

afd :: [Int] -> [(Int, Int, Int)] -> [Int] -> Int -> Bool

afd [] listaTrans listaAcc atual = elem atual listaAcc
afd (a:as) listaTrans listaAcc atual = afd as listaTrans listaAcc (trans atual a listaTrans) 

    -- Exemplo: afd [1, 0, 1, 1, 1, 1, 0] [(0, 1, 0), (0, 0, 1), (1, 1, 0), (1, 0, 2), (1, 1, 2), (2, 0, 2), (2, 1, 2)] [2] 0