-- 8

 -- Dado um caminho parcial, retorna uma lista com os possiveis movimentos a partir da ultima posicao
move :: [(Int, Int)] -> [(Int, Int)]
move [p] = makeMove p
move ps = makeMove (last ps)

makeMove :: (Int, Int) -> [(Int, Int)]
makeMove (x, y) = [(x+1, y), (x-1, y), (x, y+1), (x, y-1), (x+1, y+1), (x+1, y-1), (x-1, y+1), (x-1, y-1)]
  --

  -- Determina se uma posicao eh valida, i.e., esta dentro do 'tabuleiro'
isValid :: (Int, Int) -> Int -> Bool
isValid (x, y) n = x >= 1 && x <= n && y >= 1 && y <= n

  -- Determina se eh seguro andar para uma determinada casa, i.e., se ela nao foi visitada antes ou, caso seja a ultima do caminho, se eh igual a casa inicial
isSafe :: [(Int, Int)] -> (Int, Int) -> Int -> Bool
isSafe tmp pos dim = if (dim*dim) == (length tmp) 
                     then pos == (head tmp)
                     else (pos `elem` tmp) == False

  -- Dada uma lista, remove um elemento dado como parametro
remove :: (Eq a) => [a] -> a -> [a]
remove as e = aux [] as e
    where aux res [] e = res
          aux tmp (x:xs) e = if x == e then (tmp++xs) else aux (tmp++[x]) xs e

  -- Verifica se o caminho eh completo, i.e., passa por todas as casas sem repeticao e volta para a posicao inicial
isFull :: Int -> [(Int, Int)] -> Bool 
isFull n as = ((head as) == (last as)) && ((length as) == (n*n)+1) && check [(x, y) | x <- [1..n], y <- [1..n]] (init as)
    where check [] [] = True
          check [] as = False
          check (x:xs) ys = check xs (remove ys x)

  -- Verifica se algum caminho ja possui o tamanho 'ideal'
checkLen :: (Eq a) => [[a]] -> Int -> Bool
checkLen as n = not ((filter (\x -> (length x) == (n*n)+1) as) == [])

  -- Gera todos os caminhos possiveis
allPos :: [[(Int, Int)]] -> Int -> [[(Int, Int)]]
allPos (x:xs) n = if (checkLen (x:xs) n)
    	            then (x:xs)
    	            else allPos ([tmp++[x] | tmp <- (x:xs), x <- (move tmp), (isValid x n), (isSafe tmp x n)]) n
  
  -- Funcao 'pulo' que faz 'passeioCavaleiros' sempre retornar True
jumpAlwaysTrue :: (Int, Int) -> Int -> [(Int, Int)]
jumpAlwaysTrue ini dim = (head . (filter (isFull dim))) (allPos [[ini]] dim)

  -- Testa uma funcao 'pulo'
passeioCavaleiros :: Int -> (Int, Int) -> ((Int, Int) -> Int -> [(Int, Int)]) -> Bool
passeioCavaleiros n ini pulo = isFull n (pulo ini n)