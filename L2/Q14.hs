-- 14

import System.IO.Unsafe
import System.Random

data User = User Int Int deriving (Show)

    -- Random
rng :: (Int, Int) -> IO(Int)
rng (i, j) = do r <- randomRIO(i,j)
                return r

randomInt :: (Int, Int) -> Int
randomInt (i, j) = unsafePerformIO (rng (i, j))
    --

    -- Auxiliar p/ comparar Users: ordena primeiro pelo ranking e, em seguida pelo tempo de espera
lessThan :: User -> User -> Bool
lessThan (User r1 t1) (User r2 t2) = if r1 < r2
	                                 then True
	                                 else if r1 == r2
	                                      then t1 < t2
	                                      else False
    -- QuickSort p/ Users
qs :: [User] -> [User]
qs [] = []
qs (a:as) = (qs [x | x <- as, lessThan x a])++[a]++(qs [x | x <- as, not (lessThan x a)])

    -- Gera os usuarios aleatoriamente
generateUsers :: Int -> [User]
generateUsers 0 = []
generateUsers n = (User (randomInt (1, 25)) (randomInt (1, 100))):(generateUsers (n-1))

matchMaking :: [User] -> [(User, User)]
matchMaking [] = []
matchMaking (xs) = aux (qs xs)                      -- Ordena os usuarios
    where aux [] = []
          aux (a:b:as) = (a, b):(matchMaking as)    -- Pega dois a dois da lista ordenada

main :: IO()
main = do 
	      putStr "Digite a quantidade (deve ser par) de usuarios: ";
	      entr <- getLine
	      num <- return $ read entr
	      match <- return $ matchMaking (generateUsers num)
	      putStrLn (show match)
	      