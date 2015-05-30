-- 4

import Data.Char

caesarSalad :: [Char] -> [Int] -> [Char]
caesarSalad word key = cypher "" word (cycle key)
    where cypher secret [] _ = secret
          cypher secret (a:as) (b:bs) = if a == ' '
                                        then cypher (secret++" ") as (b:bs)
                                        else cypher (secret ++ [chr ((ord a) + b)]) as bs