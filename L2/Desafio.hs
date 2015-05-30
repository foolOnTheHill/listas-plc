-- DESAFIO

    -- Expressoes aritmeticas com associatividade a direita
data Expr = Expr Int | SumExpr Int Expr | SubExpr Int Expr | MultExpr Int Expr | DivExpr Int Expr | EqExpr Expr Expr deriving (Show, Eq) 

    -- Converte uma Expr para String
toString :: Expr -> String
toString (Expr a) = (show a)
toString (SumExpr a x) = (show a)++"+("++(toString x)++")"
toString (SubExpr a x) = (show a)++"-("++(toString x)++")"
toString (MultExpr a x) = (show a)++"*("++(toString x)++")"
toString (DivExpr a x) = (show a)++"/("++(toString x)++")"
toString (EqExpr a b) = (toString a)++"="++(toString b)

    -- Computa o resultado de uma Expr
eval :: Expr -> Int
eval (Expr a) = a
eval (SumExpr a x) = a + (eval x)
eval (SubExpr a x) = a - (eval x)
eval (MultExpr a x) = a * (eval x)
eval (DivExpr a x) = a `div` (eval x)

evalEquality :: Expr -> Bool
evalEquality (EqExpr a b) = ((eval a) == (eval b))
evalEquality _ = False

    -- Gera uma lista com todas as expressores aritmeticas com os numeros da lista de entrada, mantendo a ordem
makeExpression :: [Int] -> [Expr]
makeExpression [a] = [Expr a]
makeExpression (a:as) = [SumExpr a x | x <- tmp]++[SubExpr a x | x <- tmp]++[MultExpr a x | x <- tmp]++[DivExpr a x | x <- tmp, (eval x) /= 0]
    where tmp = makeExpression as

    -- Vai 'passando' o sinal de igualdade pela lista, gerando todas as possiveis expressoes na esquerda e na direita
makeEquality :: [Int] -> [Expr]
makeEquality (a:as) = aux [a] as
    where aux _ [] = []
          aux ys (x:xs) = [EqExpr u v | u <- (makeExpression ys), v <- (makeExpression (x:xs))]++(aux (ys++[x]) xs)

    -- Retorna uma combinacao de operadores, se existir alguma
solve :: [Int] -> String
solve xs = res
    where getCombinations = (filter (\x -> evalEquality x == True)) . makeEquality
          getFirst = toString . head
          tmp = getCombinations xs
          res = if (tmp == []) then "nao existe combinacao" else (getFirst tmp)
