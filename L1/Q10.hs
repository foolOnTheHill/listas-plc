-- 10

-- (a) map

myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (a:as) = [(f a)]++(myMap f as)

-- (b) filter

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter f (a:as) = r++(myFilter f as)
    where r = if (f a) == True then [a] else []

-- (c) foldr

myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr f a [] = a
myFoldr f a xs = myFoldr f (f lst a) tl
    where lst = last xs
          tl = init xs