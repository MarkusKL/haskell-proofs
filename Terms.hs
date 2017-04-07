
module Terms
  ( Term (term,pre)
  , fst'
  , snd'
  , only
  , print'
  , terml
  , termm
  , termr
  , term2
  ) where


class Term t where
  term :: t -> String
  pre :: t -> Integer

fst' :: Term (t a b) => t a b -> a
fst' = undefined

snd' :: Term (t a b) => t a b -> b
snd' = undefined

only :: f a -> a
only = undefined

print' :: Term t => t -> IO ()
print' = putStrLn . term

terml :: (Term (t a b), Term a) => t a b -> String
terml t
  | pre f <= pre t = "(" ++ term f ++ ")"
  | otherwise     = term f
    where f = fst' t

termm :: (Term (t a), Term a) => t a -> String
termm t
  | pre m < pre t = "(" ++ term m ++ ")"
  | otherwise     = term m
    where m = only t

termr :: (Term (t a b), Term b) => t a b -> String
termr t
  | pre s < pre t = "(" ++ term s ++ ")"
  | otherwise     = term s
    where s = snd' t

term2 :: (Term (t a b), Term a, Term b) => String -> t a b -> String
term2 s t = terml t ++ s ++ termr t
