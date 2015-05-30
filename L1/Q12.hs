-- 12

   -- Auxiliares
        -- Reverte uma string
rev :: [Char] -> [Char]
rev as = doIt "" as
    where doIt xs [] = xs
          doIt xs (a:as) = doIt (a:xs) as

checkPalindrome :: [Char] -> Bool
checkPalindrome word = (length word > 1) && word == (rev word)

filterPalindromes :: [[Char]] -> [[Char]] -> [[Char]]
filterPalindromes as [] = as
filterPalindromes as (b:bs) = if (checkPalindrome b == True) && (not (b `elem` as)) -- Evita elementos repetidos
                              then filterPalindromes (as++[b]) bs
                              else filterPalindromes as bs 

        -- Gera todas as substrings de uma palavra
allSubstrings :: [Char] -> [[Char]]
allSubstrings [] = []
allSubstrings (a:as) = (aux (a:as)) ++ allSubstrings as
    where aux [] = []
          aux (b:bs) = [b]:[(b:x) | x <- (aux bs)]

    --

longestPalindromes :: [Char] -> [[Char]]
longestPalindromes word = result
    where substr = allSubstrings word
          tmp = filterPalindromes [] substr
          mai = maximum (map length tmp)     -- Tamanho do maior palindromo
          test x = (length x) == mai         -- Funcao que testa se o tamanho eh maximo
          result = filter test tmp           -- Filtra os maiores palindromos