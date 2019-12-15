module Helpers
    ( takeEvery
    , lastN
    , ceilDiv
    , drawMap
    ) where

import qualified Data.HashMap.Strict as M


-- return every n elements of a list, but also include the last two elements
takeEvery _ [] = []
takeEvery n l = p1 ++ takeEvery n p2
  where
    (p1,p2) = helper n l
    helper 1 (l:ls) = ([l],ls)
    helper n (l:ls@(a:b:_)) = helper (n-1) ls
    helper _ ls = (ls,[])


-- return the last N entries of a list
lastN i ls = lastN' ls []
  where
    lastN' [] acc = reverse acc
    lastN' (l:ls) acc = lastN' ls (take i $ l:acc)


-- return a `div` b rounded up
ceilDiv a b = (a + b - 1) `div` b


-- transform a coordinatemap to a multiline string
drawMap draw1 m = unlines rows
  where
    sx = minimum . map fst . M.keys $ m
    ex = maximum . map fst . M.keys $ m
    sy = minimum . map snd . M.keys $ m
    ey = maximum . map snd . M.keys $ m

    charMap = M.map draw1 m
    rows = [[M.lookupDefault ' ' (x,y) charMap | x <- [sx..ex]] | y <- [sy..ey]]
