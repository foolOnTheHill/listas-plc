-- 12

    --Pilha
data MyStack t = Empty | Stack (MyStack t) (t)

pop :: MyStack t -> (t, MyStack t)
pop (Stack a b) = (b, a)

push :: t -> MyStack t -> MyStack t
push a b = Stack b a

instance (Show a) => Show(MyStack a) where 
  show Empty = ""
  show (Stack st t) = (show t)++"\n"++(show st)
     --

     -- http://en.wikipedia.org/wiki/Shunting-yard_algorithm
  -- Auxiliares
slice :: [String] -> ([String], [String])
slice as = aux [] as
    where aux tmp []     = (tmp, [])
          aux tmp (x:xs) | x /= "("  = aux (tmp++[x]) xs
                         | otherwise = (tmp, (x:xs))

preProcess :: String -> String -> [String]
preProcess tmp [] = []
preProcess tmp (a:as) | a == ' '                                = if (tmp /= []) then [tmp]++(preProcess "" as) else (preProcess "" as)
                      | a `elem` ['+', '-', '*', '/', '(', ')'] = (if (tmp /= []) then [tmp, [a]] else [[a]])++(preProcess "" as)
                      | otherwise                               = preProcess (tmp++[a]) as

invert :: [t] -> [t]
invert [] = []
invert (a:as) = (invert as)++[a]
  --

rpn :: [String] -> [String] -> [String] -> [String]
rpn o p [] = (invert o) ++ p

rpn o [] (x:xs) | x `elem` ["*", "+", "-", "("] = rpn o [x] xs
                | otherwise                     = rpn (x:o) [] xs

rpn o (p:ps) (x:xs)
    | x == "("                      = rpn o (x:p:ps) xs
    | x == ")"                      = case (slice (p:ps)) of
                                          (as, b:bs) -> rpn (as++o) bs xs
    | x `elem` ["+", "-", "*", "/"] = case (p) of
                                          "(" -> rpn o (x:p:ps) xs
                                          otherwise -> rpn (p:o) (x:ps) xs
    | otherwise                     = rpn (x:o) (p:ps) xs

convert :: String -> [String]
convert expr = rpn [] [] (preProcess "" expr)
     --

makeStack :: String -> MyStack String
makeStack expr = aux (invert (convert expr))
    where aux [] = Empty
          aux (a:as) = Stack (aux as) a