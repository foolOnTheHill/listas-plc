-- 11

data Tree t = Node t (Maybe (Tree t)) (Maybe (Tree t)) deriving (Show, Eq)

ex = Node 6 (Just (Node 2 (Just (Node 1 Nothing Nothing)) (Just (Node 4 (Just (Node 3 Nothing Nothing)) (Just (Node 5 Nothing Nothing)))))) (Just (Node 9 (Just (Node 8 Nothing Nothing)) Nothing))

--(a)

tail_inorder :: (Ord t) => Maybe (Tree t) -> [t]
tail_inorder Nothing = []
tail_inorder a = aux [] a []
    where aux [] Nothing res = res
          aux ((Just (Node a b c)):as) Nothing res = aux as c (res++[a])
          aux tmp (Just (Node a b c)) res = aux ((Just (Node a b c)):tmp) b res

-- (b) 

tail_postorder :: (Ord t) => Maybe (Tree t) -> [t]
tail_postorder Nothing = []
tail_postorder a = aux [a] []
    where aux [] scnd = scnd
          aux ((Nothing):as) scnd = aux as scnd
          aux ((Just (Node a b c)):as) scnd = aux (c:b:as) (a:scnd)
          
-- (c)

tail_preorder :: (Ord t) => Maybe (Tree t) -> [t]
tail_preorder Nothing = []
tail_preorder a = aux [a] []
    where aux [] scnd = scnd
          aux ((Nothing):as) scnd = aux as scnd
          aux ((Just (Node a b c)):as) scnd = aux (b:c:as) (scnd++[a])

-- (d)

tail_bfs :: (Ord t) => Maybe (Tree t) -> [t]
tail_bfs (Just (Node x y z)) = aux [y, z] [x]
    where aux [] scnd = scnd
          aux ((Nothing):as) scnd = aux as scnd
          aux ((Just (Node a b c)):as) scnd = aux (as++[b, c]) (scnd++[a])
