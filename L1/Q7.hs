-- 7

dropWord :: [Char] -> [Char]
dropWord [] = []
dropWord (a:as) | a == ' ' = as
                | otherwise = dropWord as

getWord :: [Char] -> [Char]
getWord [] = []
getWord (a:as) | a == ' ' = []
                | otherwise = a:(getWord as)

translate :: [Char] -> Char
translate "/" = ' '
translate ".-" = 'A'
translate "-..." = 'B'
translate "-.-." = 'C'
translate "-.." = 'D'
translate "." = 'E'
translate "..-." = 'F'
translate "--." = 'G'
translate "...." = 'H'
translate ".." = 'I'
translate ".---" = 'J'
translate "-.-" = 'K'
translate ".-.." = 'L'
translate "--" = 'M'
translate "-." = 'N'
translate "---" = 'O'
translate ".--." = 'P'
translate "--.-" = 'Q'
translate ".-." = 'R'
translate "..." = 'S'
translate "-" = 'T'
translate "..-" = 'U'
translate "...-" = 'V'
translate ".--" = 'W'
translate "-..-" = 'X'
translate "-.--" = 'Y'
translate "--.." = 'Z'
translate ".----" = '1'
translate "..---" = '2'
translate "...--" = '3'
translate "....-" = '4'
translate "....." = '5'
translate "-...." = '6'
translate "--..." = '7'
translate "---.." = '8'
translate "----." = '9'
translate "-----" = '0'
translate ".-..-." = '\"'
translate ".----." = '\''
translate "..--.." = '?'
translate "--..--" = ','
translate ".-.-.-" = '.'

morseTranslator :: [Char] -> [Char]
morseTranslator [] = []
morseTranslator str = (translate (getWord str)):(morseTranslator (dropWord str))

{- Exemplos da lista
morseTranslator "-... .- -.. / .--- --- -.- . / - .. -- ."
morseTranslator ".. / .-.. --- ...- . / .--. .-. . ... ... .. -. --. / ..-. ..... .-.-.- / .. - ... / ... --- / .-. . ..-. .-. . ... .... ..-. --."
morseTranslator ".- / ... --.- .-.. / --.- ..- . .-. -.-- / --. --- . ... / .. -. - --- / .- / -... .- .-. --..-- / .-- .- .-.. -.- ... / ..- .--. / - --- / - .-- --- / - .- -... .-.. . ... / .- -. -.. / .- ... -.- ... --..-- / .-..-. -.-. .- -. / .. / .--- --- .. -. / -.-- --- ..- ..--.. .-..-."
morseTranslator ".... --- .-- / -- .- -. -.-- / .--. .-. --- --. .-. .- -- -- . .-. ... / -.. --- . ... / .. - / - .- -.- . / - --- / -.-. .... .- -. --. . / .- / .-.. .. --. .... - / -... ..- .-.. -... ..--.. / -. --- -. . --..-- / - .... .- - ... / .- / .... .- .-. -.. .-- .- .-. . / .--. .-. --- -... .-.. . --"
morseTranslator "- --- / ..- -. -.. . .-. ... - .- -. -.. / .-- .... .- - / .-. . -.-. ..- .-. ... .. --- -. / .. ... / -.-- --- ..- / -- ..- ... - / ..-. .. .-. ... - / ..- -. -.. . .-. ... - .- -. -.. / .-. . -.-. ..- .-. ... .. --- -."
morseTranslator ".- / --. --- --- -.. / .--. ..- -. / .. ... / .. - ... / --- .-- -. / .-. . .-- --- .-. -.."
-}
