module Variables
( A
, B
, C
) where

import Data.Void (Void)

newtype A = A Void
newtype B = B Void
newtype C = C Void

instance Show A where
    show = const "A"

instance Show B where
    show = const "B"

instance Show C where
    show = const "C"
