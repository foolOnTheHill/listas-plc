-- 16

ano1 :: [Char] -> [Char]
ano1 [] = []
ano1 [a] = [a]
ano1 (a:b:as) | a == 'c' && (b == 'a' || b == 'e' || b == 'i') = 's':b:(ano1 as)
              | a == 'c' && (b == 'o' || b == 'u') = 'k':b:(ano1 as)
              | a == 's' && b == 's' = 's':(ano1 as)
              | otherwise = a:(ano1 (b:as))

ano2 :: [Char] -> [Char]
ano2 [] = []
ano2 [a] = [a]
ano2 (a:b:as) | a == 'q' && b == 'u' = 'k':(ano2 as)
              | otherwise = a:b:(ano2 as)

ano3 :: [Char] -> [Char]
ano3 [] = []
ano3 [a] = if a == 'h' then [] else [a]
ano3 (a:b:as) | a == 'c' && b == 'h' = 'x':(ano3 as)
              | a == 'n' && b == 'h' = "ni" ++ (ano3 as)
              | a == 'l' && b == 'h' = "li" ++ (ano3 as)
              | a == 'h' = ano3 (b:as)
              | otherwise = a:(ano3 (b:as))

ano4 :: [Char] -> [Char]
ano4 [] = []
ano4 [a] = [a]
ano4 (a:b:as) | a == 'j' && (b == 'a' || b == 'o') = 'x':b:(ano4 as)
              | a == 'j' && (b == 'e' || b == 'i' || b == 'u') = 'g':b:(ano4 as)
              | otherwise = a:(ano4 (b:as))

ano5 :: [Char] -> [Char]
ano5 [] = []
ano5 (a:as) | a `elem` "aeiou" = ano5 as
            | otherwise = a : ano5 as

    -- Aplica as mudancas dos anos anteriores ate o 'ano atual'
seila :: [Char] -> Int -> [Char]
seila palavra ano = aux palavra 1 ano
    where aux palavra curr ano | curr > ano = palavra
                               | curr == 1 = aux (ano1 palavra) (curr+1) ano
                               | curr == 2 = aux (ano2 palavra) (curr+1) ano
                               | curr == 3 = aux (ano3 palavra) (curr+1) ano
                               | curr == 4 = aux (ano4 palavra) (curr+1) ano
                               | curr == 5 = aux (ano5 palavra) (curr+1) ano