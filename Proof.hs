
module Proof () where

import Data.Void
import Control.Monad.Writer.Lazy

import Data.Bifunctor
--import Data.Functor.Contravariant
--import Data.Profunctor

import Variables
import Terms

type Proof = Writer [String]

axiom :: String -> Proof a
axiom s = writer (undefined,[s])

premise :: Term a => Proof a
premise = put $ axiom "præmis"

unpack :: Proof a -> a
unpack = undefined

put :: (Term a) => Proof a -> Proof a
put a = a >> axiom (term $ unpack a)

-- // Syntactic rules for the 'and' symbol \\ --

newtype And a b = And Void

andE1 :: (Term a, Term b) => And a b -> Proof a
andE1 _ = put $ axiom "andE1"

andE2 :: (Term a, Term b) => And a b -> Proof b
andE2 _ = put $ axiom "andE2"

andI :: (Term a, Term b) => a -> b -> Proof (And a b)
andI _ _ = put $ axiom "andI"


-- // Proofs using 'and' rules \\ --

proof1 :: (Term a, Term b) => And a b -> Proof (And b a)
proof1 p1 = join $ liftM2 andI (andE2 p1) (andE1 p1)

showProof :: Proof a -> String
showProof = unlines . execWriter

main :: IO ()
main = (putStr . showProof) (proof1 =<< premise :: Proof (And B A)) 
    >> putStrLn "-------------------------"
    >> (putStr . showProof) (proof2 =<< premise :: Proof (And A (And B C)))

proof2 :: (Term a, Term b, Term c) => And (And a b) c -> Proof (And a (And b c))
proof2 p1 = do
  ab <- andE1 p1
  c <- andE2 p1
  a <- andE1 ab
  b <- andE2 ab
  bc <- andI b c
  andI a bc
{-


-- // Syntactic rules for the 'or' symbol \\ --
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

-- // Functor instances \\ --

instance Bifunctor And where
  first f ab = andI (f . andE1 $ ab) (andE2 ab)
  second f ab = andI (andE1 ab) (f . andE2 $ ab)

instance Bifunctor Or where
  first f ab = orE ab (orI1 . f) (orI2)
  second f ab = orE ab (orI1) (orI2 . f)

-- ### Missing dependency
--instance Contravariant Neg where
contramap = mt

--instance Profunctor Imp where
dimap f g ab = impI $ f . impE ab . g
  

-- // Experimental printing of expressions \\ --
-}
instance (Term a, Term b) => Term (And a b) where
  term = term2 " /\\ "
  pre = const 3
{-
instance (Term a, Term b) => Term (Or a b) where
  term = term2 " \\/ "
  pre = const 2

instance (Term a) => Term (Neg a) where
  term t = "!" ++ termm t
  pre = const 4

instance (Term a, Term b) => Term (Imp a b) where
  term = term2 " -> "
  pre = const 1

instance (Term a, Term b) => Term ((->) a b) where
  term = term2 " ~> "
  pre = const 0

instance Term Bot where
  term = const "_|_"
  pre = const 5

main :: IO ()
main = print' (proof1 :: And A B -> And B A)
    >> print' (proof2 :: And (And A B) C -> And A (And B C))
    >> print' (mt :: Imp A B -> Neg B -> Neg A)
    >> print' (botE :: (Bot -> A))
    >> print' (proof3 :: Or A B -> Or B A)
    >> print' (proof6 :: Imp A (Imp A B) -> A -> B)
    >> print' (bimap :: (A -> B) -> (B -> C) -> And A B -> And B C)

-}
