module Stream
  ( Stream (..)
  , stream
  , split
  , peek
  , naturals
  ) where

data Stream a = a :~ Stream a

stream :: (a -> a) -> a -> Stream a
stream f a = a :~ (stream f (f a))

split :: Stream a -> (a,Stream a)
split (a :~ as) = (a,as)

peek :: Stream a -> (a,Stream a)
peek (a :~ as) = (a,a :~ as)

naturals :: (Num a) => Stream a
naturals = stream (+1) 1

