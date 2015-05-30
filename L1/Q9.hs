-- 9

listToNum :: Num t => (t -> t -> t) -> [t] -> t
listToNum f [a] = a
listToNum f (a:as) = f a (listToNum f as)

foldMatrix :: Num t => (t -> t -> t) -> (t -> t -> t) -> [[t]] -> t
foldMatrix _ _ [] = 0
foldMatrix f g as = listToNum g ([(listToNum f a) | a <- as])