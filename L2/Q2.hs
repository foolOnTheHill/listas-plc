-- 2

data Expr = Lit Int | SumExpr Expr Expr | SubExpr Expr Expr | MultExpr Expr Expr | DivExpr Expr Expr | ModExpr Expr Expr

eval :: Expr -> Int
eval (Lit a) = a
eval (SumExpr a b) = (eval a) + (eval b)
eval (SubExpr a b) = (eval a) - (eval b)
eval (MultExpr a b) = (eval a) * (eval b)
eval (DivExpr a b) = (eval a) `div` (eval b)
eval (ModExpr a b) = (eval a) `mod` (eval b)

-- 2+((5-2)*1)
ex1 = eval (SumExpr (Lit 2) (MultExpr (SubExpr (Lit 5) (Lit 2)) (Lit 1)))

-- 4/((2*(1-(2-2)))+2)
ex2 = eval (DivExpr (Lit 4) (SumExpr (MultExpr (Lit 2) (SubExpr (Lit 1) (SubExpr (Lit 2) (Lit 2)))) (Lit 2)))

-- (4+2) mod (2*(1+1))
ex3 = eval (ModExpr (SumExpr (Lit 4) (Lit 2)) (MultExpr (Lit 2) (SumExpr (Lit 1) (Lit 1))))

-- ((1000/10)/10)/(2+3))
ex4 = eval (DivExpr (DivExpr (DivExpr (Lit 1000) (Lit 10)) (Lit 10)) (SumExpr (Lit 2) (Lit 3)))

-- 2*(((2*2)*2)*2)
ex5 = eval (ModExpr (MultExpr (Lit 2) (MultExpr (MultExpr (MultExpr (Lit 2) (Lit 2)) (Lit 2)) (Lit 2))) (Lit 2))
