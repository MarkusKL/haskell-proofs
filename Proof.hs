{-# LANGUAGE TypeOperators #-}

module Proof ( assume
             , axiom
             , premise
             , putLine
             , showProof
             , Proof
             , type (~>)
             , extI
             , Term
             ) where

import Control.Monad.RWS.Lazy
import Data.List (intersperse)

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

evalProof :: Proof a -> Steps
evalProof = snd . \rws -> evalRWS rws () naturals
  
showProof :: Proof a -> String
showProof = showSteps . evalProof

showRefs :: [Reference] -> String
showRefs = join . intersperse ", " . fmap showRef

showRef :: Reference -> String
showRef (Left i) = show i
showRef (Right (i1,i2)) = show i1 ++ "-" ++ show i2

putLine :: IO ()
putLine = putStrLn ("\n" ++ replicate 32 '\9472' ++ "\n")

assume :: (Term a, Term b) => (a -> Proof b) -> Proof (a ~> b)
assume f = censor box $ do
  n <- state peek
  a <- axiom "antagelse" []
  b <- f a
  return (Interval n (ext b))

extI :: (a ~> b) -> (Integer,Integer)
extI (Interval a b) = (a,b)
