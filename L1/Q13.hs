-- 13
    
    -- Typeclass
class PrimeirosPrimos a where
  ehPrimo :: Num a => a -> Bool
  fib :: Num a => a -> a
  fat :: Num a => a -> a
  primeFib :: Num a => Int -> [a]
  primeFat :: Num a => Int -> [a]

    -- Instancia pra Integer
instance PrimeirosPrimos Integer where
    ehPrimo p = (p > 1) && ([x | x <- [2..(p-1)], p `mod`x  == 0] == [])
    fib 1 = 1
    fib 2 = 1
    fib n = aux (n-2) 1 1
        where aux 0 acm curr = curr
              aux n acm curr = aux (n-1) curr (acm+curr)
    fat n = foldr (*) 1 [1..n]
    primeFib n = take n (filter ehPrimo (map fib [1..]))
    primeFat n = take n (filter ehPrimo (map fat [1..]))  -- Soh tem o 2, mas ok...
    --