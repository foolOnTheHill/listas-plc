-- Q1

--1 add

add :: (Eq t) => (t, v) -> [(t, v)] -> [(t, v)]
add (chave, valor) m = (chave, valor):[(x, y) | (x, y) <- m, x /= chave]

domain :: [(t, v)] -> [t]
domain m = [x | (x, y) <- m]

image :: [(t, v)] -> [v]
image m = [y | (x, y) <- m] 

apply :: (Eq t) => t -> [(t, v)] -> (Maybe v)
apply chave [] = Nothing
apply chave ((x, y):as) = if x == chave then (Just y) else apply chave as