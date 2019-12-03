import qualified Data.Set as S
import qualified Data.List as L


main :: IO ()
main = interact (showD . solve . readD)


data Direction = DUp | DDown | DRight | DLeft
  deriving (Eq, Show)

type Point = (Int, Int)

type Wire = (Direction, Int)


readD :: String -> [[Wire]]
readD = map readWires . lines
  where
    readWires "" = []
    readWires s  = readWire s' : readWires s'''
      where
        (s', s'') = span (/= ',') s
        s'''      = dropWhile (==',') s''
    readWire ('R':s) = (DRight, read s)
    readWire ('L':s) = (DLeft, read s)
    readWire ('U':s) = (DUp, read s)
    readWire ('D':s) = (DDown, read s)


solve :: [[Wire]] -> [(Int, (Int, Int))]
solve (ws1:ws2:_) = L.sort $ zip distances intersections
  where
    ps1 = pointset ws1
    ps2 = pointset ws2
    intersections = S.toList $ S.intersection ps1 ps2
    distances = map (\(x,y) -> abs(x) + abs(y)) intersections


move :: Point -> Wire -> Point
move (x,y) (DUp,   l) = (x, y+l)
move (x,y) (DDown, l) = (x, y-l)
move (x,y) (DRight,l) = (x+l, y)
move (x,y) (DLeft, l) = (x-l, y)


pointset :: [Wire] -> S.Set Point
pointset ws = ps' (0,0) ws
  where
    ps' _ []     = S.empty
    ps' p (w:ws) = S.union (wireToPoints p w) (ps' (move p w) ws)


wireToPoints :: Point -> Wire -> S.Set Point
wireToPoints (x,y) (DUp, l) = S.fromList [(x,y+k) | k <- [0..l]]
wireToPoints (x,y) (DDown, l) = S.fromList [(x,y-k) | k <- [0..l]]
wireToPoints (x,y) (DRight, l) = S.fromList [(x+k,y) | k <- [0..l]]
wireToPoints (x,y) (DLeft, l) = S.fromList [(x-k,y) | k <- [0..l]]


showD = show . head . drop 1
