
-- 2

--(a)

data Habitantes = Elfo String | Humano String | Anao String | Hobbit String deriving (Eq)

--(b)

class Comp t where
	maisImportante :: t -> t -> Bool

--(c)

instance Comp Habitantes where
	maisImportante (Elfo n1) (Elfo n2) = n1 > n2
	maisImportante (Humano n1) (Humano n2) = n1 > n2
	maisImportante (Anao n1) (Anao n2) = n1 > n2
	maisImportante (Hobbit n1) (Hobbit n2) =  n1 > n2
	maisImportante (Elfo _) _ = True
	maisImportante (Humano _) (Elfo _) = False
	maisImportante (Humano _) _ = True
	maisImportante (Anao _) (Elfo _) = False
	maisImportante (Anao _) (Humano _) = False
	maisImportante (Anao _) _ =True
	maisImportante (Hobbit) _ = False

-- 3

--(a)

data Arvore t = No t (Maybe (Arvore t)) (Maybe (Arvore t)) deriving (Show)

--(b)

criarArvoreDeImportancia :: (Comp t) => [t] -> Maybe (Tree t)
criarArvoreDeImportancia [] = Nothing
criarArvoreDeImportancia (a:as) = insertList (Just (No a Nothing Nothing)) as
    where
    	 insertList tree [] = tree
    	 insertList (Just (No a b c)) (v:vs) = insertList (insertVal v (Just (No a b c))) vs
    	 insertVal Nothing v = (Just Node (v Nothing Nothing))
         insertVal (Just (No a b c)) v = if (maisImportante v a == True)
         	                             then (Just (No a b (insertVal c v)))
         	                             else (Just (No a (insertVal b v) c))

--(c)

filhos :: Maybe (Tree Habitantes) -> Habitantes -> [Habitantes]
filhos Nothing _ = []
filhos (Just (Node a b c)) hbt | a == hbt = toList c
                               | maisImportante hbt a == True = filhos c hbt
                               | otherwise = filhos b hbt
    where 
    	 toList Nothing = []
    	 toList (Just (Node a b c)) = [a]++(toList b)++(toList c)

-- 4

--(a) (+ 2).((*) 2).(2 /)

(+ 2).((*) 2).(2 /)

(.) : (y -> z) -> (x -> y) -> (x -> z)
(+2) :: (Num a) => a -> a
(*2) :: (Num b) => b -> b
(2/) :: (Fractional c) => c -> c -> c

-- (i) (+2).((*) 2)

(y -> z) => a -> a
y => a
z => a

(x -> y) => b -> b -> b
x => b
y => b

a => b

(x -> z) => b -> b

(+2).((*) 2) :: (Num b) => b -> b

-- (ii) (+2).((*) 2).(2 /)

(y -> z) => b -> b
y => b
z => b

(x -> y) => c -> c
x => c
y => c

(x -> z) => c -> c

(+2).((*) 2).(2 /) :: (Fractional c) => c -> c

--(b) (.).filter

(.) :: (t -> v) -> (s -> t) -> (s -> v)
filter :: (a -> Bool) -> [a] -> [a]

(y -> z) => (t -> v) -> (s -> t) -> (s -> v)
y => (t -> v)
z => (s -> t) -> (s -> v)

(x -> y) => (a -> Bool) -> [a] -> [a]
x => (a -> Bool)
y => [a] -> [a]

t => [a]
v => [a]

z => (s -> [a]) -> (s -> [a])
(x -> z) => (a -> Bool) -> (s -> [a]) -> (s -> [a]) 

(.).filter :: (a -> Bool) -> (s -> [a]) -> (s -> [a])
