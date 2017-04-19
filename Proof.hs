{-# LANGUAGE TypeOperators #-}

module Proof () where

import Control.Monad.RWS.Lazy
import Control.Monad ((>=>), (<=<))
import Data.List (intersperse)

import Variables
import Terms
import Stream
import Steps

type Proof = RWS () Steps (Stream Integer)
data (~>) a b = Interval Integer Integer
type Reference = Either Integer (Integer,Integer)

axiom :: (Term a) => String -> [Reference] -> Proof a
axiom s is = do
  n <- state split
  let t = con n
  writer (t, step n (term t) (s ++ " " ++ showRefs is))

premise :: Term a => Proof a
premise = axiom "præmis" []

-- // Syntactic rules for the 'and' symbol \\ --

newtype And a b = And Integer

andE1 :: (Term a, Term b) => And a b -> Proof a
andE1 t1 = axiom "andE1" [Left $ ext t1]

andE2 :: (Term a, Term b) => And a b -> Proof b
andE2 t1 = axiom "andE2" [Left $ ext t1]

andI :: (Term a, Term b) => a -> b -> Proof (And a b)
andI t1 t2 = axiom "andI" [Left $ ext t1, Left $ ext t2]


-- // Proofs using 'and' rules \\ --

proof1 :: (Term a, Term b) => And a b -> Proof (And b a)
proof1 p1 = do
  a <- andE1 p1
  b <- andE2 p1
  andI b a

evalProof :: Proof a -> Steps
evalProof = snd . \rws -> evalRWS rws () naturals
  
showProof :: Proof a -> String
showProof = showSteps . evalProof

showRefs :: [Reference] -> String
showRefs = join . intersperse ", " . fmap showRef

showRef :: Reference -> String
showRef (Left i) = show i
showRef (Right (i1,i2)) = show i1 ++ "-" ++ show i2


main :: IO ()
main = (putStr . showProof) (proof1 =<< premise :: Proof (And B A)) 
    >> putLine
    >> (putStr . showProof) (proof2 =<< premise :: Proof (And A (And B C)))
    >> putLine
    >> (putStr . showProof) (proof3 =<< premise :: Proof (Or B A))
    >> putLine
    >> (putStr . showProof) (join $ liftM2 proof5 premise premise :: Proof (Or (And A C) B))
    >> putLine
    >> (putStr . showProof) (join $ liftM2 proof6 premise (premise :: Proof A) :: Proof B)
    >> putLine
    >> (putStr . showProof) (negnegI =<< premise :: Proof (Neg (Neg A)))
    >> putLine
    >> (putStr . showProof) (Proof.exp =<< premise :: Proof (Imp A (Imp B C)))

putLine :: IO ()
putLine = putStrLn ("\n" ++ replicate 32 '\9472' ++ "\n")

proof2 :: (Term a, Term b, Term c) => And (And a b) c -> Proof (And a (And b c))
proof2 p1 = do
  ab <- andE1 p1
  c <- andE2 p1
  a <- andE1 ab
  b <- andE2 ab
  bc <- andI b c
  andI a bc


-- // Syntactic rules for the 'or' symbol \\ --
newtype Or a b = Or Integer

orI1 :: (Term a, Term b) => a -> Proof (Or a b)
orI1 t1 = axiom "orI1" [Left $ ext t1]

orI2 :: (Term a, Term b) => b -> Proof (Or a b)
orI2 t1 = axiom "orI2" [Left $ ext t1]

orE :: (Term a, Term b, Term c) => Or a b -> (a ~> c) -> (b ~> c) -> Proof c
orE t1 p1 p2 = axiom "orE" [Left $ ext t1, Right $ extI p1, Right $ extI p2]


-- // Proofs using 'or' rule \\ --

proof3 :: (Term a, Term b) => Or a b -> Proof (Or b a)
proof3 t = do
  p1 <- assume orI2
  p2 <- assume orI1
  orE t p1 p2

--proof4 :: (Term a, Term b) => Or a b -> (a -> Proof c) -> Proof (Or c b)
--proof4 p1 p2 = orE p1 (orI1 <=< p2) orI2
  

proof5 :: (Term a, Term b, Term c) => Or a b -> c -> Proof (Or (And a c) b)
proof5 ab c = do
  p1 <- assume (\a -> do
                   ac <- andI a c
                   orI1 ac
               )
  p2 <- assume orI2
  orE ab p1 p2

assume :: (Term a, Term b) => (a -> Proof b) -> Proof (a ~> b)
assume f = censor box $ do
  --writer (undefined,[("/=======","=======\\")])
  n <- state peek
  a <- axiom "antagelse" []
  b <- f a
  return (Interval n (ext b))
  --writer (Interval n (ext b),[("\\=======","=======/")])

extI :: (a ~> b) -> (Integer,Integer)
extI (Interval a b) = (a,b)

-- // Syntax rules for implication ('imp') sybmol \\ --

newtype Imp a b = Imp Integer

impI :: (Term a, Term b) => (a ~> b) -> Proof (Imp a b)
impI p1 = axiom "impI" [Right $ extI p1]

impE :: (Term a, Term b) => Imp a b -> a -> Proof b
impE t1 t2 = axiom "impE" [Left $ ext t1, Left $ ext t2]


-- // Proofs using implication ('imp') rules \\ --

proof6 :: (Term a, Term b) => Imp a (Imp a b) -> a -> Proof b
proof6 aab a = do
  ab <- impE aab a
  impE ab a

-- // Syntax rules for bottom and negation ('bot' and 'neg') symbols \\ --

newtype Bot = Bot Integer

botE :: Term a => Bot -> Proof a
botE t1 = axiom "botE" [Left $ ext t1]


newtype Neg a = Neg Integer

negI :: Term a => (a ~> Bot) -> Proof (Neg a)
negI p1 = axiom "negI" [Right $ extI p1]

negE :: Term a => And a (Neg a) -> Proof Bot
negE t1 = axiom "negE" [Left $ ext t1]


-- // Law of the excluded middle (LEM) \\ --

lem :: Term a => Proof (Or a (Neg a))
lem = axiom "LEM" []


-- // Proofs using bottom and negation \\ --

negnegI :: Term a => a -> Proof (Neg (Neg a))
negnegI a = assume (andI a >=> negE) >>= negI

exp :: (Term a, Term b, Term c) => Imp (And a b) c -> Proof (Imp a (Imp b c))
exp ab'c = do
  impI =<< assume (\a -> impI =<< assume (\b -> do
                                             ab <- andI a b
                                             impE ab'c ab
                                             ))

-- // Proof of modus tollens (MT) \\ --


{-
mt :: (Term p, Term q) => Imp p q -> Neg q -> Neg p
mt ipq nq = negI (\p -> negE $ andI (impE ipq p) nq)
-}

-- // Experimental printing of expressions \\ --

instance (Term a, Term b) => Term (And a b) where
  term = term2 " /\\ "
  pre = const 3
  con = And
  ext (And i) = i

instance (Term a, Term b) => Term (Or a b) where
  term = term2 " \\/ "
  pre = const 2
  con = Or
  ext (Or i) = i

instance (Term a, Term b) => Term (Imp a b) where
  term = term2 " -> "
  pre = const 1
  con = Imp
  ext (Imp i) = i

instance (Term a) => Term (Neg a) where
  term t = "¬" ++ termm t
  pre = const 4
  con = Neg
  ext (Neg i) = i

instance Term Bot where
  term = const "_|_"
  pre = const 5
  con = Bot
  ext (Bot i) = i
{-
main :: IO ()
main = print' (proof1 :: And A B -> And B A)
    >> print' (proof2 :: And (And A B) C -> And A (And B C))
    >> print' (mt :: Imp A B -> Neg B -> Neg A)
    >> print' (botE :: (Bot -> A))
    >> print' (proof3 :: Or A B -> Or B A)
    >> print' (proof6 :: Imp A (Imp A B) -> A -> B)
    >> print' (bimap :: (A -> B) -> (B -> C) -> And A B -> And B C)

-}
