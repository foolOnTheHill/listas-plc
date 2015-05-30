-- 15

import Data.Char

dv11 :: Int -> Int -> [Int] -> Int
dv11 ini fim as = (dv ini as fim) `mod` 11
    where tmp = zip [ini..] as                           -- Lista com pares na forma (fator, digito_cpf)
          dv k as s = sum [x*y | (x, y) <- tmp, x <= s]  -- Somatorio multiplicando pelo fator correspondente

cpfToInt :: [Char] -> [Int]
cpfToInt [] = []
cpfToInt (a:as) | a == '.' || a == '-' = cpfToInt as
                | otherwise = ((ord a) - (ord '0')):(cpfToInt as)

checkCpf :: [Char] -> Bool
checkCpf cpf | length intcpf /= 11 = False
             | otherwise = (dec == fat1) && (elev == fat2)
        where intcpf = cpfToInt cpf
              dec = head (drop 9 intcpf)    -- 10º digito
              elev = head (drop 10 intcpf)  -- 11º digito 
              fat1 = dv11 1 9 intcpf        -- DV mod 11 dos 9 primeiros digitos
              fat2 = dv11 0 9 intcpf        -- DV mod 11 dos 10 primeiros digitos

cpfilter :: [[Char]] -> [[Char]]
cpfilter cp = filter checkCpf cp