-- 8

{-

Bem, te enviei um email perguntando qual a abordagem pra essa questão, mas n obtive resposta. Daí, fiz das duas formas. :P

Só uma observação: Acredito que seja p/ retornar uma lista contendo números COPRIMOS DOIS A DOIS,
pq se fosse pra o mdc de todos juntos ser 1, bastava a lista ter dois números cujo mdc seja 1, pois dado que o cálculo do mdc pra 
uma lista é feito pegando dois a dois e acumulando o resultado (vide a implementação da funcao mdcl) e que o mdc de 1 e qualquer
outro inteiro é também é 1.

-}


-- Primeira forma (o mdc de todos é 1)

{-
    --- Auxiliares
mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

mdcl :: [Int] -> [Int]
mdcl [] = []
mdcl [x] = [x]    
mdcl (x:y:z) = mdcl ([mdc x y] ++ z)

allSubsets :: [Int] -> [[Int]]
allSubsets [] = []
allSubsets (a:as) = [a]:[(a:x) | x <- aux] ++ aux
    where aux = allSubsets as

coprimesl :: [Int] -> Bool
coprimesl as = (length as) > 1 && (mdcl as) == [1]

getMax :: [[Int]] -> [Int]
getMax as = result
    where setPairs ls = (length ls, ls)
          tmp = map setPairs as
          mai = if tmp == [] then (0, []) else maximum tmp
          result = snd mai
    ---

getCoprimes :: [Int] -> [Int]
getCoprimes as = result
    where tmp = filter coprimesl (allSubsets as)
          result = getMax tmp
-}

-- Outra forma (são coprimos 2 a 2)

filterPrimos :: [Int] -> [Int]
filterPrimos ls = clean ls ls

    --- Auxiliares
mdc :: Int -> Int -> Int
mdc a 0 = a
mdc a b = mdc b (a `mod` b)

clean :: [Int] -> [Int] -> [Int]
clean [] bs = bs
clean (a:as) bs = if not (elem a bs)
                  then clean as bs
                  else clean as [x | x <- bs, (mdc x  a) == 1 || x == a]
    -- 
    