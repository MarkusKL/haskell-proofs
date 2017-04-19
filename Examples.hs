module Examples (
                ) where

import Control.Monad ((>=>), (<=<), join)

import Proof
import Rules
import Variables

main :: IO ()
main = (putStr . showProof) (proof1 =<< premise :: Proof (And B A)) 
    >> putLine
    >> (putStr . showProof) (proof2 =<< premise :: Proof (And A (And B C)))
    >> putLine
    >> (putStr . showProof) (proof3 =<< premise :: Proof (Or B A))
    >> putLine
    >> (putStr . showProof) (join $ proof5 <$> premise <*> premise :: Proof (Or (And A C) B))
    >> putLine
    >> (putStr . showProof) (join $ proof6 <$> premise <*> (premise :: Proof A) :: Proof B)
    >> putLine
    >> (putStr . showProof) (negnegI =<< premise :: Proof (Neg (Neg A)))
    >> putLine
    >> (putStr . showProof) (curry' =<< premise :: Proof (Imp A (Imp B C)))
    >> putLine
    >> (putStr . showProof) (join $ mt <$> premise <*> (premise :: Proof (Neg Q)) :: Proof (Neg P))

proof1 :: (Term a, Term b) => And a b -> Proof (And b a)
proof1 p1 = do
  a <- andE1 p1
  b <- andE2 p1
  andI b a

proof2 :: (Term a, Term b, Term c) => And (And a b) c -> Proof (And a (And b c))
proof2 p1 = do
  ab <- andE1 p1
  c <- andE2 p1
  a <- andE1 ab
  b <- andE2 ab
  bc <- andI b c
  andI a bc

proof3 :: (Term a, Term b) => Or a b -> Proof (Or b a)
proof3 t = do
  p1 <- assume orI2
  p2 <- assume orI1
  orE t p1 p2

proof5 :: (Term a, Term b, Term c) => Or a b -> c -> Proof (Or (And a c) b)
proof5 ab c = do
  p1 <- assume (\a -> do
                   ac <- andI a c
                   orI1 ac
               )
  p2 <- assume orI2
  orE ab p1 p2

proof6 :: (Term a, Term b) => Imp a (Imp a b) -> a -> Proof b
proof6 aab a = do
  ab <- impE aab a
  impE ab a

negnegI :: Term a => a -> Proof (Neg (Neg a))
negnegI a = assume (andI a >=> negE) >>= negI

curry' :: (Term a, Term b, Term c) => Imp (And a b) c -> Proof (Imp a (Imp b c))
curry' ab'c = do
  impI =<< assume (\a -> impI =<< assume (\b -> do
                                             ab <- andI a b
                                             impE ab'c ab
                                             ))

-- // Proof of modus tollens (MT) \\ --

mt :: (Term p, Term q) => Imp p q -> Neg q -> Proof (Neg p)
mt ipq nq = do
  negI =<< (assume $ \p -> do
           q <- impE ipq p
           qnq <- andI q nq
           negE qnq
       )
