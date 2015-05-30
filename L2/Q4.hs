-- 4

-- Termos: c (simbolo de constante) ou f(t1, t2,...) (simbolo de funcao aplicado a termos)
data Term = Constant Char | FuncTerm Char [Term]

-- Sentenca Atomica: t1 = t2 ou R(t1, t2,...) (simbolo de relacao/predicado aplicado a termos)
data AtomicSentence = EqSentence Term Term | PredSentence Char [Term]

-- Sentenca: a ^ b, a v b, a -> b, ¬a, a
data Sentence = AndSentence Sentence Sentence | OrSentence Sentence Sentence | ImpSentence Sentence Sentence | NegSentence Sentence | Atm AtomicSentence

-- Mapeia simbolos de relacao/predicado em funcoes booleanas (a relacao recebe uma lista de parametros)
data Predicate t = Pred (Char -> ([t] -> Bool))

-- Mapeia os simbolos de funcao (a funcao recebe uma lista de parametros)
data Function t = Func (Char -> ([t] -> t))

-- Mapeia os simbolos de constante em eltos do dominio
data ConsSimbolTo t = Map (Char -> t)

data Interpretation t = Interp ((Predicate t), (Function t), (ConsSimbolTo t))

termValue :: Term -> Interpretation t -> t
termValue (Constant a) (Interp ((Pred pr), (Func fc), (Map mp))) = mp a
termValue (FuncTerm g as) (Interp ((Pred pr), (Func fc), (Map mp))) = (fc g) (map parc as)
    where parc = \x -> termValue x (Interp ((Pred pr), (Func fc), (Map mp)))

atomicValue :: (Eq t) => AtomicSentence -> Interpretation t -> Bool
atomicValue (EqSentence t1 t2) interp = (termValue t1 interp) == (termValue t2 interp)
atomicValue (PredSentence rel as) (Interp ((Pred pr), (Func fc), (Map mp))) = (pr rel) (map parc as)
    where parc = \x -> termValue x (Interp ((Pred pr), (Func fc), (Map mp)))

value :: (Eq t) => Sentence -> Interpretation t -> Bool
value (Atm at) interp = atomicValue at interp
value (AndSentence psi phi) interp = (value psi interp) && (value phi interp)
value (OrSentence psi phi) interp = (value psi interp) || (value phi interp)
value (ImpSentence psi phi) interp = (not (value psi interp)) || (value phi interp)
value (NegSentence psi) interp = (not (value psi interp))

{- -- Exemplo
st1 = AndSentence (Atm (EqSentence (Constant 'a') (Constant 'b'))) (Atm (PredSentence 'R' [(FuncTerm 's' [(Constant 'c')])]))

pr 'R' = (\[x] -> (x `mod` 2) == 0) 
fc 's' = (\[x] -> (x+1))
cs 'a' = 1
cs 'b' = 1
cs 'c' = 1

intp = Interp ((Pred pr), (Func fc), (Map cs))
res = value st1 intp
-}