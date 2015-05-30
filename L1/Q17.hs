-- 17

aux :: Ord t => [t] -> [t] -> [t] -> [t]
aux r [] [] = []
aux r [] q = r++q -- Evita fazer mais que max {len(m), len(n)} comparações
aux r q [] = r++q -- /\ that
aux r (a:as) (b:bs) = if a <= b
                      then aux (r++[a]) as (b:bs)
                      else aux (r++[b]) (a:as) bs

merge :: Ord t => [t] -> [t] -> [t]
merge m n = aux [] m n