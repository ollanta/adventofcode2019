import qualified Data.Set as S
import qualified Data.List as L


main :: IO ()
main = interact (showD . solve . readD)


type Direction = (Int, Int)

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
    readWire ('R':s) = ((1, 0), read s)
    readWire ('L':s) = ((-1,0), read s)
    readWire ('U':s) = ((0, 1), read s)
    readWire ('D':s) = ((0,-1), read s)


solve :: [[Wire]] -> [(Int, (Int, Int))]
solve (ws1:ws2:_) = L.sort $ zip distances intersections
  where
    ps1 = pointset ws1
    ps2 = pointset ws2
    intersections = S.toList $ S.intersection ps1 ps2
    distances = map (\(x,y) -> abs(x) + abs(y)) intersections


move :: Point -> Wire -> Point
move (x,y) ((dx,dy),l) = (x+dx*l, y+dy*l)


pointset :: [Wire] -> S.Set Point
pointset ws = ps' (0,0) ws
  where
    ps' _ []     = S.empty
    ps' p (w:ws) = S.union (wireToPoints p w) (ps' (move p w) ws)


wireToPoints :: Point -> Wire -> S.Set Point
wireToPoints (x,y) ((dx, dy), l)
  | dx == 0 = S.fromList [(x,y') | y' <- [y,y+dy..y+dy*l]]
  | dy == 0 = S.fromList [(x',y) | x' <- [x,x+dx..x+dx*l]]


showD = show . head . drop 1
