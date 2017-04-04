
module Proof () where

import Data.Void
import Data.Functor.Identity (Identity, runIdentity)


-- // Syntactic rules for the 'and' symbol \\ --

newtype And a b = And Void

andE1 :: And a b -> a
andE1 = undefined

andE2 :: And a b -> b
andE2 = undefined

andI :: a -> b -> And a b
andI = undefined


-- // Proofs using 'and' rules \\ --

proof1 :: And a b -> And b a
proof1 p1 = andI (andE2 p1) (andE1 p1)

proof2 :: And (And a b) c -> And a (And b c)
proof2 p1 = runIdentity $ do
  let ab = andE1 p1
  let c = andE2 p1
  let a = andE1 ab
  let b = andE2 ab
  return $ andI a (andI b c)


-- // Syntactic rules for the 'or' symbol // --

newtype Or a b = Or Void

orI1 :: a -> Or a b
orI1 = undefined

orI2 :: b -> Or a b
orI2 = undefined

orE :: Or a b -> (a -> c) -> (b -> c) -> c
orE = undefined


-- // Proofs using 'or' rulse \\ --

proof3 :: Or a b -> Or b a
proof3 p1 = orE p1 orI2 orI1

proof4 :: Or a b -> (a -> c) -> Or c b
proof4 p1 p2 = orE p1 (orI1 . p2) orI2

proof5 :: Or a b -> c -> Or (And a c) b
proof5 p1 p2 = runIdentity $ do
  let l1 = \a -> andI a p2
  let l2 = proof4 p1 l1
  return l2

-- // Syntax rules for implication ('imp') sybmol \\ --

newtype Imp a b = Imp Void

impI :: (a -> b) -> Imp a b
impI = undefined

impE :: Imp a b -> a -> b
impE = undefined

-- // Proofs using implication ('imp') rules \\ --

proof6 :: Imp a (Imp a b) -> a -> b
proof6 p1 p2 = impE (impE p1 p2) p2

-- // Syntax rules for bottom and negation ('bot' and 'neg') symbols \\ --

newtype Bot = Bot Void

botE :: Bot -> a
botE = undefined

newtype Neg a = Neg Void

negI :: (a -> Bot) -> Neg a
negI = undefined

negE :: And a (Neg a) -> Bot
negE = undefined

-- // Law of the excluded middle (LEM) \\ --

lem :: Or a (Neg a)
lem = undefined

-- // Proofs using bottom and negation \\ --

negnegI :: a -> Neg (Neg a)
negnegI p1 = negI (negE . andI p1)

-- // Proof of modus tollens (MT) \\ --
mt :: Imp p q -> Neg q -> Neg p
mt ipq nq = negI (\p -> negE $ andI (impE ipq p) nq)


-- // Experimental printing of expressions \\ --

newtype A = A ()
newtype B = B ()
newtype C = C ()

instance Show A where
  show _ = "A"

instance Show B where
  show _ = "B"

instance Show C where
  show _ = "C"

fst' :: f a b -> a
fst' = undefined

snd' :: f a b -> b
snd' = undefined

instance (Show a, Show b) => Show (And a b) where
  show t = "( " ++ show (fst' t) ++ " \\/ " ++ show (snd' t) ++ " )"

print1 :: And A B -> And B A
print1 = proof1

print2 :: And (And A B) C -> And A (And B C)
print2 = proof2

main :: IO ()
main = print (print1 undefined) >> print (print2 undefined)
