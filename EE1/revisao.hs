-- (a) ((:).foldr.foldr) (:)

(.) :: (y -> z) -> (x -> y) -> (x -> z)
(:) :: a -> [a] -> [a]
foldr :: (t -> u -> u) -> u -> [t] -> u

-- (i) (:).foldr 

(y -> z) => a -> [a] -> [a]
y => a
z => [a] -> [a]

(x -> y) => (t -> u -> u) -> u -> [t] -> u
x => (t -> u -> u)
y => u -> [t] -> u

a => u -> [u] -> u
z => [u -> [t] -> u] -> [u -> [t] -> u]

(x -> z) => (t -> u -> u) -> ([u -> [t] -> u] -> [u -> [t] -> u])

(:).foldr :: (t -> u -> u) -> ([u -> [t] -> u] -> [u -> [t] -> u])

-- (ii) (:).foldr.foldr

(y -> z) => (t -> u -> u) -> ([u -> [t] -> u] -> [u -> [t] -> u])
y => (t -> u -> u)
z => [u -> [t] -> u] -> [u -> [t] -> u]

(x -> y) => (m -> n -> n) -> n -> [m] -> n
x => (m -> n -> n)
y => n -> [m] -> n

t => n
u => [m]
u => n
n => [m]

x => (m -> [m] -> [m])
z => [[m] -> [[m]] -> [m]] -> [[m] -> [[m]] -> [m]]

x -> z => (m -> [m] -> [m]) -> [[m] -> [[m]] -> [m]] -> [[m] -> [[m]] -> [m]]
(:).foldr.foldr => (m -> [m] -> [m]) -> [[m] -> [[m]] -> [m]] -> [[m] -> [[m]] -> [m]]

-- (iii) ((:).foldr.foldr) (:)

(:) :: a -> [a] -> [a]

m => a
[m] => [a]
[m] => [a]

((:).foldr.foldr) (:) => (a -> [a] -> [a]) -> [[a] -> [[a]] -> [a]] -> [[a] -> [[a]] -> [a]]

-- (b) map.map.foldr

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

-- (c) foldr.foldr.foldl

(.) :: (y -> z) -> (x -> y) -> (y -> z)
foldr :: (a -> b -> b) -> b -> [a] -> b
foldr :: (t -> u -> u) -> u -> [t] -> u
foldl :: (s -> v -> s) -> s -> [v] -> s

-- (i) foldr.foldr

(y -> z) => (a -> b -> b) -> b -> [a] -> b
y => (a -> b -> b)
z => b -> [a] -> b

(x -> y) => (t -> u -> u) -> u -> [t] -> u
x => (t -> u -> u)
y => u -> [t] -> u

a => u
b => [t]
b => u
u => [t]

z => [t] -> [[t]] -> [t]
(x -> z) => (t -> [t] -> [t]) -> [t] -> [[t]] -> [t]

foldr.foldr :: (t -> [t] -> [t]) -> [t] -> [[t]] -> [t]

-- (ii) foldr.foldr.foldl

(.) :: (y -> z) -> (x -> y) -> (x -> z)
foldl :: (s -> v -> s) -> s -> [v] -> s

(y -> z) => (t -> [t] -> [t]) -> [t] -> [[t]] -> [t]
y => (t -> [t] -> [t])
z => [t] -> [[t]] -> [t]

(x -> y) => (s -> v -> s) -> s -> [v] -> s
x => (s -> v -> s)
y => s -> [v] -> s

t => s
[t] => [v]
[t] => s

-- Erro! s = t e [t] = s ??????

-- (d) foldl.foldr.foldl

(.) :: (y -> z) -> (x -> y) -> (x -> z)
foldl :: (a -> b -> a) -> a -> [b] -> a
foldr :: (m -> n -> n) -> n -> [m] -> n
foldl :: (s -> t -> s) -> s -> [t] -> s

-- (i) foldl.foldr

(y -> z) => (a -> b -> a) -> a -> [b] -> a
y => (a -> b -> a)
z => a -> [b] -> a

(x -> y) => (m -> n -> n) -> n -> [m] -> n
x => (m -> n -> n)
y => n -> [m] -> n

a => n
b => [m]
z => n -> [[m]] -> n

(x -> z) => (m -> n -> n) -> n -> [[m]] -> n

-- (ii) foldl.foldr.foldl

(y -> z) => (m -> n -> n) -> n -> [[m]] -> n
y => (m -> n -> n)
z => n -> [[m]] -> n

(x -> y) => (s -> t -> s) -> s -> [t] -> s
x => (s -> t -> s)
y => s -> [t] -> s

m => s
n => [t]
n => s
s => [t]

z => ([t] -> [[[t]]] -> [t])
x => ([t] -> t -> [t])

(x -> z) => ([t] -> t -> [t]) -> [t] -> [[[t]]] -> [t]

-- (e) map.(foldr(/2))

(.) :: (y -> z) -> (x -> y) -> (x -> z)
foldr :: (m -> n -> n) -> n -> [m] -> n
map :: (a -> b) -> [a] -> [b]

-- (i) foldr (/2)

(/2) :: (Fractional t) => t -> t

(m -> n -> n) => (t -> t)
t => m
t => (n -> n)

(foldr (/2)) :: (Fractional t) => n -> [(n -> n)] -> n

-- (ii) map.(foldr (/2))

(y -> z) => (a -> b) -> [a] -> [b]
y => (a -> b)
z => [a] -> [b]

(x -> y) => n -> [(n -> n)] -> n
x => n
y => [(n -> n)] -> n

a => [(n -> n)]
b => n
z => [[(n -> n)]] -> [n]

(x -> z) => n -> [[(n -> n)]] -> [n]
map.(foldr (/2)) :: (Fractional (n -> n)) => n -> [[(n -> n)]] -> [n]