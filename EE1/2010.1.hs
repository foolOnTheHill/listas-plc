-- 2

data Tree t = NilT | Node t (Tree t) (Tree t) deriving (Show)

dfs :: (Show t) => (Tree t) -> IO()
dfs NilT = do return ()
dfs (Node a b c) = do putStr (show a)
                      printTree [b, c]

printTree :: (Show t) => [(Tree t)] -> IO()
printTree [] = do putStr ".\n"
printTree ((NilT):as) = printTree as
printTree ((Node a b c):as) = do putStr ", "
                                 putStr (show a)
                                 printTree ([b, c]++as)

ex = Node 10 (Node 7 NilT NilT) (Node 4 (Node 12 NilT (Node 14 NilT NilT)) (Node 6 (Node 2 NilT NilT) (Node 8 NilT NilT)))

-- 3

-- (a) map.map.foldr
{-
(.) :: (y -> z) -> (x -> y) -> (x -> z)
map :: (a -> b) -> [a] -> [b]
foldr :: (c -> d -> d) -> d -> [c] -> d

-- (i) map.map

(y -> z) => (a -> b) -> [a] -> [b]
y => (a -> b)
z => [a] -> [b]

(x -> y) => (t -> v) -> [t] -> [v]
x => (t -> v)
y -> [t] -> [v]

a => [t]
b => [v]

z => [[t]] -> [[v]]
(x -> z) => (t -> v) -> [[t]] -> [[v]]

-- (ii) map.map.foldr

(.) :: (y -> z) -> (x -> y) -> (x -> z)

(y -> z) => (t -> v) -> [[t]] -> [[v]]
y => (t -> v)
z => [[t]] -> [[v]]

(x -> y) => (c -> d -> d) -> d -> [c] -> d
x => (c -> d -> d)
y -> d -> [c] -> d

t => d
v => [c] -> d
z => [[d]] -> [[[c] -> d]]

(x -> z) => (c -> d -> d) -> [[d]] -> [[[c] -> d]]
-}

-- (b) map.((.) (foldr (++) (foldr (++) [] [[1], [2], [4,5,6], [3]])))
{-
(foldr (++) [] [[1], [2], [4,5,6], [3]]) :: [Int]

map.((.) (foldr (++) (foldr (++) [] [[1], [2], [4,5,6], [3]]))) => map.((.) (foldr (++) [Int]))

map.((.) (foldr (++) [Int]))

(.) :: (y -> z) -> (x -> y) -> (x -> z)
foldr :: (t -> u -> u) -> u -> [t] -> u
map :: (a -> b) -> [a] -> [b]
(++) :: [v] -> [v] -> [v]

-- foldr (++) [Int]

(t -> u -> u) => [v] -> [v] -> [v]
t => [v]
u => [v]

foldr (++) :: [v] -> [[v]] -> [v]
foldr (++) [Int] :: [[Int]] -> [Int]

-- (.) foldr (++) [Int]
(.) (y -> z) -> (x -> y) -> (x -> z)

(y -> z) => [[Int]] -> [Int]
y => [[Int]]
z => [Int]

(.) foldr (++) [Int] => (p -> [[Int]]) -> (p -> [Int])

-- map.((.) foldr (++) [Int])

(y -> z) => (a -> b) -> [a] -> [b]
y => (a -> b)
z => [a] -> [b]

(x -> y) => (p -> [[Int]]) -> (p -> [Int])
x => (p -> [[Int]])
y => (p -> [Int])

a => p
b => [Int]
z => [p] -> [[Int]]

(x -> z) => (p -> [[Int]]) -> [p] -> [[Int]]

map.((.) foldr (++) [Int]) :: (p -> [[Int]]) -> [p] -> [[Int]]
-}

-- 4

data No t = No t [t]
data Grafo t = Grafo [No t]

getNeighborhood :: (Eq t) => [(No t)] -> t -> [t]
getNeighborhood [] _ = []
getNeighborhood ((No v adj):graph) no = if v == no
	                                    then adj
	                                    else getNeighborhood graph no

mapEdges :: (Int -> Int -> Int) -> (Grafo Int) -> (Int -> [Int])
mapEdges f (Grafo graph) no = [f no x | x <- adj]
    where adj = getNeighborhood graph no

gr :: Grafo Int
gr = Grafo [(No 10 [7, 4]), (No 4 [12, 6]), (No 12 [14]), (No 6 [2, 8])]