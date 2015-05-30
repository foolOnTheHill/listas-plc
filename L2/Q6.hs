-- 6

data Cell = X | O | Nil deriving (Eq) -- Representa uma celula do tabuleiro.
data Game = Game [Cell]               -- O tabuleiro eh uma lista com 9 celulas, portanto, a localização da jogada eh dada por um numero entre 1 e 9

showCell :: Cell -> String
showCell X = "X"
showCell O = "O"
showCell Nil = "-"

printGame :: Game -> IO()
printGame (Game gm) = putStrLn tab
    where [a, b, c, d, e, f, g, h, i] = map showCell gm
          tab = a++" | "++b++" | "++c++"\n"++d++" | "++e++" | "++f++"\n"++g++" | "++h++" | "++i

-- Verifica se alguem venceu
checkWin :: Game -> Bool
checkWin (Game [a, b, c, d, e, f, g, h, i]) = res
    where line1 = ((a == X) && (b == X) && (c == X)) || ((a == O) && (b == O) && (c == O))
          line2 = ((d == X) && (e == X) && (f == X)) || ((d == O) && (e == O) && (f == O))
          line3 = ((g == X) && (h == X) && (i == X)) || ((g == O) && (h == O) && (i == O))
          col1  = ((a == X) && (d == X) && (g == X)) || ((a == O) && (d == O) && (g == O))
          col2  = ((b == X) && (e == X) && (h == X)) || ((b == O) && (e == O) && (h == O))
          col3  = ((c == X) && (f == X) && (i == X)) || ((c == O) && (f == O) && (i == O))
          diag1 = ((a == X) && (e == X) && (i == X)) || ((a == O) && (e == O) && (i == O))
          diag2 = ((c == X) && (e == X) && (g == X)) || ((c == O) && (e == O) && (g == O))
          res = line1 || line2 || line3 || col1 || col2 || col3 || diag1 || diag2

-- Retorna quem venceu
getWinner :: Game -> String
getWinner (Game [a, b, c, d, e, f, g, h, i]) = res
    where line1X = (a == X) && (b == X) && (c == X)
          line2X = (d == X) && (e == X) && (f == X)
          line3X = (g == X) && (h == X) && (i == X)
          col1X  = (a == X) && (d == X) && (g == X)
          col2X  = (b == X) && (e == X) && (h == X)
          col3X  = (c == X) && (f == X) && (i == X)
          diag1X = (a == X) && (e == X) && (i == X)
          diag2X = (c == X) && (e == X) && (g == X)
          res = if (line1X || line2X || line3X || col1X || col2X || col3X || diag1X || diag2X) then "Voce" else "O Computador"

-- Verifica se deu empate
checkDraw :: Game -> Bool
checkDraw (Game as) = res
    where count [] = 0
          count (x:xs) = if (x == Nil) 
          	             then 1 + (count xs)
          	             else (count xs)
          res = (count as) == 0

-- Verifica se eh uma jogada valida
isValid :: Game -> Int -> Bool
isValid (Game as) pos = pos >= 1 && pos <= 9 && (head (drop (pos-1) as)) == Nil

getPos :: String -> Int
getPos "1" = 1
getPos "2" = 2
getPos "3" = 3
getPos "4" = 4
getPos "5" = 5
getPos "6" = 6
getPos "7" = 7
getPos "8" = 8
getPos "9" = 9
getPos _   = 20

-- Coloca um 'X' na posicao escolhida pelo jogador
makeMove :: Game -> Int -> Game
makeMove (Game as) pos = Game ((take (pos-1) as)++[X]++(drop pos as))

-- Faz um movimento de acordo com a dificuldade escolhida
computerTurn :: Game -> String -> Game
computerTurn as diff = if (diff == "facil")
                       then easy as
                       else hard as

-- Pode perder: se 'defende' e escolhe posicoes aleatorias
easy :: Game -> Game
easy (Game [a, b, c, d, e, f, g, h, i]) = Game tmp
    where tmp = if a == O && b == O && c == Nil
    	  then [a, b, O, d, e, f, g, h, i]
    	  else if a == O && b == Nil && c == O
        then [a, O, c, d, e, f, g, h, i]
        else if a == Nil && b == O && c == O
        then [O, b, c, d, e, f, g, h, i]

    	  else if d == O && e == O && f == Nil
    	  then [a, b, c, d, e, O, g, h, i]
    	  else if d == O && e == Nil && f == O
        then [a, b, c, d, O, f, g, h, i]
        else if d == Nil && e == O && f == O
        then [a, b, c, O, e, f, g, h, i]

    	  else if g == O && h == O && i == Nil
    	  then [a, b, c, d, e, f, g, h, O]
    	  else if g == O && h == Nil && i == O
        then [a, b, c, d, e, f, g, O, i]
        else if g == Nil && h == O && i == O
        then [a, b, c, d, e, f, O, h, i]

        else if a == X && b == X && c == Nil
    	  then [a, b, O, d, e, f, g, h, i]
    	  else if a == X && b == Nil && c == X
        then [a, O, c, d, e, f, g, h, i]
        else if a == Nil && b == X && c == X
        then [O, b, c, d, e, f, g, h, i]

    	  else if d == X && e == X && f == Nil
    	  then [a, b, c, d, e, O, g, h, i]
    	  else if d == X && e == Nil && f == X
        then [a, b, c, d, O, f, g, h, i]
        else if d == Nil && e == X && f == X
        then [a, b, c, O, e, f, g, h, i]

    	  else if g == X && h == X && i == Nil
    	  then [a, b, c, d, e, f, g, h, O]
    	  else if g == X && h == Nil && i == X
        then [a, b, c, d, e, f, g, O, i]
        else if g == Nil && h == X && i == X
        then [a, b, c, d, e, f, O, h, i]

        else if a == O && d == O && g == Nil
        then [a, b, c, d, e, f, O, h, i]
        else if a == O && d == Nil && g == O
        then [a, b, c, O, e, f, g, h, i]
        else if a == Nil && d == O && g == O
        then [O, b, c, d, e, f, g, h, i]

        else if b == O && e == O && h == Nil
        then [a, b, c, d, e, f, g, O, i]
        else if b == O && e == Nil && h == O
        then [a, b, c, d, O, f, g, h, i]
        else if b == Nil && e == O && h == O
        then [a, O, c, d, e, f, g, h, i]

        else if c == O && f == O && i == Nil
        then [a, b, c, d, e, f, g, h, O]
        else if c == O && f == Nil && i == O
        then [a, b, c, d, e, O, g, h, i]
        else if c == Nil && f == O && i == O
        then [a, b, O, d, e, f, g, h, i]

        else if a == X && d == X && g == Nil
        then [a, b, c, d, e, f, O, h, i]
        else if a == X && d == Nil && g == X
        then [a, b, c, O, e, f, g, h, i]
        else if a == Nil && d == X && g == X
        then [O, b, c, d, e, f, g, h, i]
        
        else if b == X && e == X && h == Nil
        then [a, b, c, d, e, f, g, O, i]
        else if b == X && e == Nil && h == X
        then [a, b, c, d, O, f, g, h, i]
        else if b == Nil && e == X && h == X
        then [a, O, c, d, e, f, g, h, i]

        else if c == X && f == X && i == Nil
        then [a, b, c, d, e, f, g, h, O]
        else if c == X && f == Nil && i == X
        then [a, b, c, d, e, O, g, h, i]
        else if c == Nil && f == X && i == X
        then [a, b, O, d, e, f, g, h, i]

        else if a == O && e == O && i == Nil
        then [a, b, c, d, e, f, g, h, O]
        else if a == O && e == Nil && i == O
        then [a, b, c, d, O, f, g, h, i]
        else if a == Nil && e == O && i == O
        then [O, b, c, d, e, f, g, h, i]

        else if a == X && e == X && i == Nil
        then [a, b, c, d, e, f, g, h, O]
        else if a == X && e == Nil && i == X
        then [a, b, c, d, O, f, g, h, i]
        else if a == Nil && e == X && i == X
        then [O, b, c, d, e, f, g, h, i]

        else if c == O && e == O && g == Nil
        then [a, b, c, d, e, f, O, h, i]
        else if c == O && e == Nil && g == O
        then [a, b, c, d, O, f, g, h, i]
        else if c == Nil && e == O && g == O
        then [a, b, O, d, e, f, g, h, i]

        else if c == X && e == X && g == Nil
        then [a, b, c, d, e, f, O, h, i]
        else if c == X && e == Nil && g == X
        then [a, b, c, d, O, f, g, h, i]
        else if c == Nil && e == X && g == X
        then [a, b, O, d, e, f, g, h, i]

        else if a == Nil
        then [O, b, c, d, e, f, g, h, i]
        else if c == Nil
        then [a, b, O, d, e, f, g, h, i]
        else if g == Nil
        then [a, b, c, d, e, f, O, h, i]
        else if i == Nil
        then [a, b, c, d, e, f, g, h, O]
        else if b == Nil
        then [a, O, c, d, e, f, g, h, i]
        else if d == Nil
        then [a, b, c, O, e, f, g, h, i]
        else if f == Nil
        then [a, b, c, d, e, O, g, h, i]
        else [a, b, c, d, e, f, g, O, i]

-- Usa uma estrategia otima e nunca perde
hard :: Game -> Game
hard (Game [a, b, c, d, e, f, g, h, i]) = Game tmp
    where tmp = if [a, b, c, d, e, f, g, h, i] == [Nil, Nil, Nil, Nil, Nil, Nil, Nil, Nil, Nil]
    	  then [Nil, Nil, Nil, Nil, O, Nil, Nil, Nil, Nil]
    	  else if [a, b, c, d, e, f, g, h, i] == [Nil, X, Nil, Nil, O, Nil, Nil, Nil, Nil]
    	  then [Nil, X, Nil, Nil, O, Nil, Nil, Nil, O]
    	  else if [a, b, c, d, e, f, g, h, i] == [Nil, Nil, Nil, Nil, O, X, Nil, Nil, Nil]
    	  then [Nil, Nil, Nil, Nil, O, X, O, Nil, Nil]
    	  else if [a, b, c, d, e, f, g, h, i] == [Nil, Nil, Nil, X, O, Nil, Nil, Nil, Nil]
    	  then [Nil, Nil, Nil, X, O, Nil, Nil, Nil, O]
    	  else if [a, b, c, d, e, f, g, h, i] == [Nil, Nil, Nil, Nil, O, Nil, Nil, X, Nil]
    	  then [O, Nil, Nil, Nil, O, Nil, Nil, X, Nil]

    	  else if a == O && b == O && c == Nil
    	  then [a, b, O, d, e, f, g, h, i]
    	  else if a == O && b == Nil && c == O
        then [a, O, c, d, e, f, g, h, i]
        else if a == Nil && b == O && c == O
        then [O, b, c, d, e, f, g, h, i]

    	  else if d == O && e == O && f == Nil
    	  then [a, b, c, d, e, O, g, h, i]
    	  else if d == O && e == Nil && f == O
        then [a, b, c, d, O, f, g, h, i]
        else if d == Nil && e == O && f == O
        then [a, b, c, O, e, f, g, h, i]

    	  else if g == O && h == O && i == Nil
    	  then [a, b, c, d, e, f, g, h, O]
    	  else if g == O && h == Nil && i == O
        then [a, b, c, d, e, f, g, O, i]
        else if g == Nil && h == O && i == O
        then [a, b, c, d, e, f, O, h, i]

        else if a == X && b == X && c == Nil
    	  then [a, b, O, d, e, f, g, h, i]
    	  else if a == X && b == Nil && c == X
        then [a, O, c, d, e, f, g, h, i]
        else if a == Nil && b == X && c == X
        then [O, b, c, d, e, f, g, h, i]

    	  else if d == X && e == X && f == Nil
    	  then [a, b, c, d, e, O, g, h, i]
    	  else if d == X && e == Nil && f == X
        then [a, b, c, d, O, f, g, h, i]
        else if d == Nil && e == X && f == X
        then [a, b, c, O, e, f, g, h, i]

    	  else if g == X && h == X && i == Nil
    	  then [a, b, c, d, e, f, g, h, O]
    	  else if g == X && h == Nil && i == X
        then [a, b, c, d, e, f, g, O, i]
        else if g == Nil && h == X && i == X
        then [a, b, c, d, e, f, O, h, i]

        else if a == O && d == O && g == Nil
        then [a, b, c, d, e, f, O, h, i]
        else if a == O && d == Nil && g == O
        then [a, b, c, O, e, f, g, h, i]
        else if a == Nil && d == O && g == O
        then [O, b, c, d, e, f, g, h, i]

        else if b == O && e == O && h == Nil
        then [a, b, c, d, e, f, g, O, i]
        else if b == O && e == Nil && h == O
        then [a, b, c, d, O, f, g, h, i]
        else if b == Nil && e == O && h == O
        then [a, O, c, d, e, f, g, h, i]

        else if c == O && f == O && i == Nil
        then [a, b, c, d, e, f, g, h, O]
        else if c == O && f == Nil && i == O
        then [a, b, c, d, e, O, g, h, i]
        else if c == Nil && f == O && i == O
        then [a, b, O, d, e, f, g, h, i]

        else if a == X && d == X && g == Nil
        then [a, b, c, d, e, f, O, h, i]
        else if a == X && d == Nil && g == X
        then [a, b, c, O, e, f, g, h, i]
        else if a == Nil && d == X && g == X
        then [O, b, c, d, e, f, g, h, i]
        
        else if b == X && e == X && h == Nil
        then [a, b, c, d, e, f, g, O, i]
        else if b == X && e == Nil && h == X
        then [a, b, c, d, O, f, g, h, i]
        else if b == Nil && e == X && h == X
        then [a, O, c, d, e, f, g, h, i]

        else if c == X && f == X && i == Nil
        then [a, b, c, d, e, f, g, h, O]
        else if c == X && f == Nil && i == X
        then [a, b, c, d, e, O, g, h, i]
        else if c == Nil && f == X && i == X
        then [a, b, O, d, e, f, g, h, i]

        else if a == O && e == O && i == Nil
        then [a, b, c, d, e, f, g, h, O]
        else if a == O && e == Nil && i == O
        then [a, b, c, d, O, f, g, h, i]
        else if a == Nil && e == O && i == O
        then [O, b, c, d, e, f, g, h, i]

        else if a == X && e == X && i == Nil
        then [a, b, c, d, e, f, g, h, O]
        else if a == X && e == Nil && i == X
        then [a, b, c, d, O, f, g, h, i]
        else if a == Nil && e == X && i == X
        then [O, b, c, d, e, f, g, h, i]

        else if c == O && e == O && g == Nil
        then [a, b, c, d, e, f, O, h, i]
        else if c == O && e == Nil && g == O
        then [a, b, c, d, O, f, g, h, i]
        else if c == Nil && e == O && g == O
        then [a, b, O, d, e, f, g, h, i]

        else if c == X && e == X && g == Nil
        then [a, b, c, d, e, f, O, h, i]
        else if c == X && e == Nil && g == X
        then [a, b, c, d, O, f, g, h, i]
        else if c == Nil && e == X && g == X
        then [a, b, O, d, e, f, g, h, i]

        else if a == Nil
        then [O, b, c, d, e, f, g, h, i]
        else if c == Nil
        then [a, b, O, d, e, f, g, h, i]
        else if g == Nil
        then [a, b, c, d, e, f, O, h, i]
        else if i == Nil
        then [a, b, c, d, e, f, g, h, O]
        else if b == Nil
        then [a, O, c, d, e, f, g, h, i]
        else if d == Nil
        then [a, b, c, O, e, f, g, h, i]
        else if f == Nil
        then [a, b, c, d, e, O, g, h, i]
        else [a, b, c, d, e, f, g, O, i]

jogo :: Game -> String -> Bool -> IO ()
jogo tab diff inv = do 
                       -- Se não for a volta de um movimento invalido, o computador joga
                       let newBoard = if (inv == True) then tab else (computerTurn tab diff);
                       printGame newBoard; -- imprime o estado atual
                       if (checkWin newBoard) == True
                       then do putStrLn ((getWinner newBoard)++" venceu!");
                       else if (checkDraw newBoard == True)
                       	    then putStrLn "\nEmpate!\n";
                       	    else do putStrLn "\nEscolha uma posicao (1 a 9) para jogar!\n";
                           	   	    entr <- getLine;
                           	   	    if (isValid newBoard (getPos entr)) == True -- verifica se eh valido
                           	   	    then jogo (makeMove newBoard (getPos entr)) diff False; -- passa para o proximo turno
                           	   	    else do putStrLn "\nMovimento invalido!\n"
                           	   	            jogo newBoard diff True -- se for um movimento invalido, manda o jogador escolher uma nova posicao
                      
jogoDaVelha :: String -> IO()
jogoDaVelha diff = jogo (Game [Nil, Nil, Nil, Nil, Nil, Nil, Nil, Nil, Nil]) diff False
