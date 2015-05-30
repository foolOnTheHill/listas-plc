-- 10

import System.IO.Unsafe
import System.Random

data Pino = Azul | Vermelho | Verde | Cinza | Roxo | Rosa | Preto | Branco deriving (Show, Eq)
    
    -- Random
rng :: (Int, Int) -> IO(Int)
rng (i, j) = do r <- randomRIO(i,j)
                return r

getRandomInt :: (Int, Int) -> Int
getRandomInt (i, j) = unsafePerformIO (rng (i, j))
    --

getPino :: Int -> Pino
getPino 1 = Azul
getPino 2 = Vermelho
getPino 3 = Verde
getPino 4 = Cinza
getPino 5 = Roxo
getPino 6 = Rosa

    -- Seleciona um pino aleatoriamente a partir de uma lista dada como parametro
selectRandom :: [Pino] -> (Pino, [Pino])
selectRandom as = res
    where i = getRandomInt (1, (length as)-1)
          pino = head (drop (i-1) as)
          next = (take (i-1) as)++(drop i as)
          res = (pino, next)

    -- Retorna uma sequencia com 4 pinos aleatorios
getRandomSequence :: [Pino] -> [Pino] -> [Pino]
getRandomSequence tmp colors = if (length tmp == 4)
    	                       then tmp
    	                       else getRandomSequence (tmp++[pino]) next
    where (pino, next) = selectRandom colors

getSequence :: [Pino]
getSequence = getRandomSequence [] [Azul, Vermelho, Verde, Roxo, Rosa, Cinza]
    --

    -- Retorna o pino correspondente
mapColors :: String -> Pino
mapColors "az" = Azul
mapColors "vm" = Vermelho
mapColors "vr" = Verde
mapColors "rx" = Roxo
mapColors "rs" = Rosa
mapColors "cz" = Cinza

    -- Retorna a lista com pinos pretos ou brancos, dependendo da resposta
getReturn :: [Pino] -> [Pino] -> [String] -> [String]
getReturn _ _ [] = []
getReturn tmp (a:as) (x:xs) = if (mapColors x) == a
                              then "pr":(getReturn (tmp++[a]) as xs)
                              else if (mapColors x) `elem` (tmp++(a:as))
                              	   then "br":(getReturn (tmp++[a]) as xs)
                              	   else "-":(getReturn (tmp++[a]) as xs)

senha :: [Pino] -> Int -> IO()
senha sqc turn = do
                   putStr "Turnos restantes: "
                   putStr (show turn)
                   putStr "\nEscolha as cores! (az, vm, vr, rx, rs, cz)\nPrimeira: "
                   prim <- getLine
                   putStr "Segunda: "
                   seg <- getLine
                   putStr "Terceira: "
                   ter <- getLine
                   putStr "Quarta: "
                   qrta <- getLine
                   res <- return $ getReturn [] sqc [prim, seg, ter, qrta]
                   putStr "Resultado: "
                   putStrLn (show res)
                   if res == ["pr", "pr", "pr", "pr"]
                   then putStrLn "Você venceu! :)"
                   else if (turn - 1) == 0
                   	    then putStrLn "Fim do jogo."
                   	    else senha (sqc) (turn-1)

jogoSenha :: IO()
jogoSenha = do 
	           putStr "Digite a quantidade de turnos: "
	           tmp <- getLine
	           turn <- return $ read tmp
	           senha (getSequence) turn
