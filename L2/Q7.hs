-- 7

qs :: (Ord t) => [t] -> [t]
qs [] = []
qs (a:as) = (qs [x | x <- as, x < a])++[a]++(qs [x | x <- as, x > a]) -- Sem o <= ou >= para evitar duplicados 

generateFree :: Int -> [(Int, Int)]
generateFree 1 = [] -- Nao possui nos para se conectar
generateFree 2 = [(1, 2), (2, 1)]  -- Faz as 2 conexoes possiveis
generateFree n = qs (aux 3 n [(1, 2), (2, 1)])
    where aux x y res = if x > y
    	                then res
    	                else aux (x+1) y res++[(1, x), (x, 1), (2, x), (x, 2)] -- Conecta aos nos "mais conectados"

getSafeNodes :: Int -> [(Int, Int)]
getSafeNodes i = [(a, i), (b, i), (c, i), (i, a), (i, b), (i, c)]
          -- Calcula como se fosse uma lista circular
    where a = (i-1)
          b = (i+1) `mod` i
          c = (i+2) `mod` i

generateRegular :: Int -> [(Int, Int)]
generateRegular n = qs (aux 2 n [])
    where aux x y res = if x > y
    	                then res
    	                else if x == 2
    	                	 then aux (x+1) y (res++[(1, 2), (2, 1)])
    	                	 else aux (x+1) y (res++(getSafeNodes x))

printNetwork :: FilePath -> [(Int, Int)] -> IO()
printNetwork path [] = do appendFile path ""
printNetwork path ((x, y):as) = do appendFile path (show x)
                                   appendFile path ","
                                   appendFile path (show y)
                                   appendFile path "\n"
                                   printNetwork path as
               
modeloMisto :: (Ord t, Fractional t) => Int -> t -> FilePath -> IO()
modeloMisto n p path = printNetwork path net
    where net = if p >= 1
    	        then generateFree n
    	        else generateRegular n
