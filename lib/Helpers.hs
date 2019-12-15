module Helpers
    ( takeEvery
    , ceilDiv
    ) where

-- return every n elements of a list, but also include the last two elements
takeEvery _ [] = []
takeEvery n l = p1 ++ takeEvery n p2
  where
    (p1,p2) = helper n l
    helper 1 (l:ls) = ([l],ls)
    helper n (l:ls@(a:b:_)) = helper (n-1) ls
    helper _ ls = (ls,[])


-- return a `div` b rounded up
ceilDiv a b = (a + b - 1) `div` b
