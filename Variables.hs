module Variables
( A
, B
, C
) where

import Data.Void (Void)

import Terms

newtype A = A Void
newtype B = B Void
newtype C = C Void

instance Term A where
    term = const "A"
    pre = const 5

instance Term B where
    term = const "B"
    pre = const 5

instance Term C where
    term = const "C"
    pre = const 5

