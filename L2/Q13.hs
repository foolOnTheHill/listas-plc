-- 13

data Card = Lacaio String Int Int Int | Feitico String Int Int deriving (Show)
data CardCampo = LacaioCampo Card deriving (Show)
data Jogador = Jogador String [Card] Int Int deriving (Show)
data Campo = Campo [CardCampo] [CardCampo] deriving (Show)

    -- Ordena os cards pelo custo de mana, colocando os feiticos por ultimo
sortCards :: [Card] -> [Card]
sortCards xs = res
    where filterFeiticos (Feitico _ _ _) = True
          filterFeiticos (Lacaio _ _ _ _) = False
          todosFeiticos = filter filterFeiticos xs
          todosLacaios = filter (not . filterFeiticos) xs
          qs [] = []
          qs ((Lacaio nome custo atk vida):as) = (qs [(Lacaio nx cx ax vx) | (Lacaio nx cx ax vx) <- as, cx <= custo])++[(Lacaio nome custo atk vida)]++(qs [(Lacaio ny cy ay vy) | (Lacaio ny cy ay vy) <- as, cy > custo])
          res = (qs todosLacaios) ++ todosFeiticos

    -- Retorna uma tripla com: cards que serao jogados (custo total de mana menor ou igual ao disponivel), cards restantes e o custo total
playCards :: Int -> [Card] -> ([Card], [Card], Int)
playCards avMana cards = aux 0 [] cards
    where aux curr res [] = (res, [], curr)
          aux curr res ((Feitico nome custo dano):as) = (res, ((Feitico nome custo dano):as), curr)
          aux curr res ((Lacaio nome custo atk vida):as) = if (curr+custo) <= avMana
                                                           then aux (curr+custo) (res++[(Lacaio nome custo atk vida)]) as
                                                           else (res, ((Lacaio nome custo atk vida):as), curr)

turn :: (Jogador, Jogador, Campo) -> (Jogador, Jogador, Campo)
turn ((Jogador nome1 cards1 mana1 vida1), (Jogador nome2 cards2 mana2 vida2), (Campo campo1 campo2)) = ((Jogador nome1 nwCards1 nwMana1 nwLife1), (Jogador nome2 nwCards2 nwMana2 nwLife2), (Campo nwCampo1 nwCampo2))
    where getAttack = sum . map (\(LacaioCampo (Lacaio _ _ atk _)) -> atk)
          attack_pl1 = getAttack campo1
          attack_pl2 = getAttack campo2
          nwLife1 = vida1 - attack_pl2
          nwLife2 = vida2 - attack_pl1
          (tmpCampo1, nwCards1, tmpMana1) = playCards mana1 (sortCards cards1)
          (tmpCampo2, nwCards2, tmpMana2) = playCards mana2 (sortCards cards2)
          nwCampo1 = campo1 ++ (map (\x -> (LacaioCampo x)) tmpCampo1)
          nwCampo2 = campo2 ++ (map (\x -> (LacaioCampo x)) tmpCampo2)
          nwMana1 = mana1 - tmpMana1
          nwMana2 = mana2 - tmpMana2

 -- Exemplo

jg1 = (Jogador "Tyrion" [(Feitico "F1" 1 2), (Lacaio "Lc1" 5 2 1), (Lacaio "Lc2" 1 2 1), (Lacaio "Lc3" 3 2 1)]) 4 10
jg2 = (Jogador "Arya" [(Lacaio "Lc4" 4 2 1), (Feitico "F1" 1 2), (Lacaio "Lc5" 1 2 1), (Lacaio "Lc6" 5 2 1)]) 12 10
cmp = (Campo [(LacaioCampo (Lacaio "Lc7" 2 7 1))] [])