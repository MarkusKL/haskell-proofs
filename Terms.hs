
module Terms
  ( Term (term)
  , fst'
  , snd'
  , only
  ) where

fst' :: Term (t a b) => t a b -> a
fst' = undefined

snd' :: Term (t a b) => t a b -> b
snd' = undefined

only :: f a -> a
only = undefined

class Term t where
  term :: t -> String
  -- precedence :: e -> Integer ?
