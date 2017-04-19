module Variables
( A
, B
, C
, P
, Q
) where

import Terms

newtype A = A Integer
newtype B = B Integer
newtype C = C Integer
newtype P = P Integer
newtype Q = Q Integer

instance Term A where
    term = const "A"
    pre = const 5
    con = A
    ext (A i) = i

instance Term B where
    term = const "B"
    pre = const 5
    con = B
    ext (B i) = i

instance Term C where
    term = const "C"
    pre = const 5
    con = C
    ext (C i) = i

instance Term P where
    term = const "P"
    pre = const 5
    con = P
    ext (P i) = i

instance Term Q where
    term = const "Q"
    pre = const 5
    con = Q
    ext (Q i) = i
