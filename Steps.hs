
module Steps
  ( Steps
  , step
  , box
  , showSteps
  ) where

type Steps = [Step]
data Step = Step Integer String String | Box Steps 

showSteps :: Steps -> String
showSteps s = showI 0 (longest s) s

showI :: Int -> Int -> Steps -> String
showI _ _ [] = ""
showI n l ((Step i s1 s2):xs) = concat [show i, " ", boxlines, s1, space, boxlines, " ", s2, "\n", showI n l xs]
  where indent = l - length s1 - n * 2
        boxlines = replicate n ver
        space = replicate indent ' '
showI n l ((Box ss):xs) = concat [boxtop n l, showI (n+1) l ss, boxbot n l, showI n l xs]
  where boxlines = replicate n ver 

boxtop :: Int -> Int -> String
boxtop n l = concat ["  ", boxlines, return nw, replicate (l-2-2*n) hor, return ne, boxlines, "\n"]
  where boxlines = replicate n ver 

boxbot :: Int -> Int -> String
boxbot n l = concat ["  ", boxlines, return sw, replicate (l-2-2*n) hor, return se, boxlines, "\n"]
  where boxlines = replicate n ver 

step :: Integer -> String -> String -> Steps
step = ((.) . (.) . (.)) return Step

box :: Steps -> Steps
box = return . Box

longest :: Steps -> Int
longest (Step _ s _ : xs) = max (length s) (longest xs)
longest (Box xs : ys) = max (longest xs + 2) (longest ys)
longest [] = 0

hor = '\9472'
ver = '\9474'
nw = '\9484'
ne = '\9488'
sw = '\9492'
se = '\9496'

