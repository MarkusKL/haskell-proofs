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

instance Term B where
    term = const "B"

instance Term C where
    term = const "C"
