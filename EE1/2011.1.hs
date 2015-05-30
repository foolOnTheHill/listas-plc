
-- 1

--(a)

data Tree t = Node t (Maybe (Tree t)) (Maybe (Tree t)) deriving (Eq, Ord)

--(b)

buildSearchTree :: (Ord t) => [t] -> Maybe (Tree t)
buildSearchTree [] = Nothing
buildSearchTree (a:as) = insertList (Just (Node a Nothing Nothing)) as
    where insertList tree [] = tree
          insertList tree (v:vs) = insertList (insertVal v tree) vs
          insertVal v Nothing = (Just (Node v Nothing Nothing))
          insertVal v (Just (Node x y z)) = if (v >= x)
          	                                then insertVal v z
          	                                else insertVal v y

--(c)

searchTreeSort :: (Ord t) => [t] -> [t]
searchTreeSort as = res
    where tree = buildSearchTree as
          inorder Nothing = []
          inorder (Just (Node a b c)) = (inorder b)++[a]++(inorder c)
          res = inorder tree

-- 2

--(a) f::Ord a => [a] -> (a -> [b]) -> [[b]]
--f.map (+1)

--(b) ((:).foldr.foldr) (:)

