-- 3

data Tree t = Node t (Maybe (Tree t)) (Maybe (Tree t)) deriving (Show, Eq)

    -- Exemplo da questao	
tr = Node 8 (Just (Node 3 (Just (Node 1 Nothing Nothing)) (Just (Node 6 (Just (Node 4 Nothing Nothing)) (Just (Node 7 Nothing Nothing)))))) (Just (Node 10 Nothing (Just (Node 14 (Just (Node 13 Nothing Nothing)) Nothing))))

-- (a)

trim :: (Ord t) => t -> t -> Maybe (Tree t) -> Maybe (Tree t)
trim mi ma Nothing = Nothing
trim mi ma (Just (Node a b c)) | a >= mi && a <= ma = Just (Node a (trim mi ma b) (trim mi ma c)) -- Se a raiz da subarvore atual estiver no intervalo, a mantemos e continuamos podando as suas subarvores a direita e a esquerda.
                               | a < mi = (trim mi ma c) -- Como eh uma arvore binaria, caso a raiz da subarvore atual seja menor que o valor minimo, continua podando a partir da subarvore a direita
	                             | a > ma = (trim mi ma b) -- Ideia parecida com a de cima.

-- (b)

    -- Percorre as subarvores da direita e da esquerda, verificando se elas possuem a mesma estrutura
symmetric :: (Tree t) -> Bool
symmetric (Node a b c) = aux b c
    where aux Nothing Nothing = True
          aux _ Nothing = False
          aux Nothing _ = False
          aux (Just (Node x y z)) (Just (Node p q r)) = (aux y q) && (aux z r)

  -- Exemplo simetrico:
--sm = Node 2 (Just (Node 1 Nothing Nothing)) (Just (Node 2 Nothing Nothing))

  -- Exemplo nao-simetrico:
--nsm = Node 2 (Just (Node 1 (Just (Node 2 Nothing Nothing)) Nothing)) (Just (Node 2 Nothing Nothing))

-- (c)

isomorphic :: (Eq t) => (Tree t) -> (Tree t) -> Bool
isomorphic t1 t2 = aux (Just t1) (Just t2)
    where aux Nothing Nothing = True
          aux _ Nothing = False
          aux Nothing _ = False
          aux (Just (Node a b c)) (Just (Node x y z)) = (a == x) && (((aux b y) && (aux c z)) || ((aux b z) && (aux c y)))	-- Tenta fazer as rotações e verifica se o resultado eh igual a primeira arvore

   -- Exemplo da questao
--t1 = Node 1 (Just (Node 2 (Just (Node 4 Nothing Nothing)) (Just (Node 5 (Just (Node 7 Nothing Nothing)) (Just (Node 8 Nothing Nothing)))))) (Just (Node 3 (Just (Node 6 Nothing Nothing)) Nothing))
--t2 = Node 1 (Just (Node 3 Nothing (Just (Node 6 Nothing Nothing)))) (Just (Node 2 (Just (Node 4 Nothing Nothing)) (Just (Node 5 (Just (Node 8 Nothing Nothing)) (Just (Node 7 Nothing Nothing)))))) 
