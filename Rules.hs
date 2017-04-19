{-# LANGUAGE TypeOperators #-}

module Rules ( And
             , andI
             , andE1
             , andE2
             , Or
             , orI1
             , orI2
             , orE
             , Imp
             , impI
             , impE
             , Bot
             , botE
             , Neg
             , negI
             , negE
             ) where

import Terms
import Proof


-- // Syntactic rules for the 'and' symbol \\ --

newtype And a b = And Integer

andE1 :: (Term a, Term b) => And a b -> Proof a
andE1 t1 = axiom "andE1" [Left $ ext t1]

andE2 :: (Term a, Term b) => And a b -> Proof b
andE2 t1 = axiom "andE2" [Left $ ext t1]

andI :: (Term a, Term b) => a -> b -> Proof (And a b)
andI t1 t2 = axiom "andI" [Left $ ext t1, Left $ ext t2]

instance (Term a, Term b) => Term (And a b) where
  term = term2 " /\\ "
  pre = const 3
  con = And
  ext (And i) = i


-- // Syntactic rules for the 'or' symbol \\ --

newtype Or a b = Or Integer

orI1 :: (Term a, Term b) => a -> Proof (Or a b)
orI1 t1 = axiom "orI1" [Left $ ext t1]

orI2 :: (Term a, Term b) => b -> Proof (Or a b)
orI2 t1 = axiom "orI2" [Left $ ext t1]

orE :: (Term a, Term b, Term c) => Or a b -> (a ~> c) -> (b ~> c) -> Proof c
orE t1 p1 p2 = axiom "orE" [Left $ ext t1, Right $ extI p1, Right $ extI p2]

instance (Term a, Term b) => Term (Or a b) where
  term = term2 " \\/ "
  pre = const 2
  con = Or
  ext (Or i) = i


-- // Syntax rules for implication ('imp') sybmol \\ --

newtype Imp a b = Imp Integer

impI :: (Term a, Term b) => (a ~> b) -> Proof (Imp a b)
impI p1 = axiom "impI" [Right $ extI p1]

impE :: (Term a, Term b) => Imp a b -> a -> Proof b
impE t1 t2 = axiom "impE" [Left $ ext t1, Left $ ext t2]

instance (Term a, Term b) => Term (Imp a b) where
  term = term2 " -> "
  pre = const 1
  con = Imp
  ext (Imp i) = i
  

-- // Syntax rules for bottom ('bot') \\ --

newtype Bot = Bot Integer

botE :: Term a => Bot -> Proof a
botE t1 = axiom "botE" [Left $ ext t1]

instance Term Bot where
  term = const "_|_"
  pre = const 5
  con = Bot
  ext (Bot i) = i


-- // Syntax rules for negation ('neg') \\ --

newtype Neg a = Neg Integer

negI :: Term a => (a ~> Bot) -> Proof (Neg a)
negI p1 = axiom "negI" [Right $ extI p1]

negE :: Term a => And a (Neg a) -> Proof Bot
negE t1 = axiom "negE" [Left $ ext t1]

instance (Term a) => Term (Neg a) where
  term t = "¬" ++ termm t
  pre = const 4
  con = Neg
  ext (Neg i) = i


-- // Law of the excluded middle (LEM) \\ --

lem :: Term a => Proof (Or a (Neg a))
lem = axiom "LEM" []
