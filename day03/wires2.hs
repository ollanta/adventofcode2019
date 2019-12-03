import Data.Set as S
import Data.List as L
import Prelude as P


main :: IO ()
main = interact (showD . solve . readD)


data Direction = DUp | DDown | DRight | DLeft
  deriving (Eq, Show)


type Point = (Int, Int)

type Line = (Direction, Int)


readD :: String -> [[Line]]
readD = P.map readLines . lines
  where
    readLines "" = []
    readLines s  = readL s' : readLines s'''
      where
        (s', s'') = span (/= ',') s
        s'''      = dropWhile (==',') s''
    readL ('R':s) = (DRight, read s)
    readL ('L':s) = (DLeft, read s)
    readL ('U':s) = (DUp, read s)
    readL ('D':s) = (DDown, read s)


solve (ls1:ls2:_) = L.sort ds
  where
    ps1 = pointset ls1
    ps2 = pointset ls2
    intersections = S.toList $ S.intersection ps1 ps2
    ds1 = P.map (\p -> distanceTo p (0,0) ls1) intersections
    ds2 = P.map (\p -> distanceTo p (0,0) ls2) intersections
    ds = zipWith (\a b -> (a+b,a,b)) ds1 ds2


distanceTo :: Point -> Point -> [Line] -> Int
distanceTo p@(px,py) o@(ox,oy) (line@(dir, l):ls)
  | oy == py && dir == DRight && ox <= px && ox+l >= px = px-ox
  | oy == py && dir == DLeft &&  ox >= px && ox-l <= px = ox-px
  | ox == px && dir == DUp &&    oy <= py && oy+l >= py = py-oy
  | ox == px && dir == DDown &&  oy >= py && oy-l <= py = oy-py
  | otherwise = l + distanceTo p (move o line) ls
  

move (x,y) (DUp, l) = (x, y+l)
move (x,y) (DDown, l) = (x, y-l)
move (x,y) (DRight, l) = (x+l, y)
move (x,y) (DLeft, l) = (x-l, y)


pointset :: [Line] -> Set Point
pointset ls = ps' (0,0) ls
  where
    ps' _ []     = S.empty
    ps' p (l:ls) = S.union (pl p l) (ps' (move p l) ls)
      where
        
        pl (x,y) (DUp, l) = S.fromList [(x,y+k) | k <- [0..l]]
        pl (x,y) (DDown, l) = S.fromList [(x,y-k) | k <- [0..l]]
        pl (x,y) (DRight, l) = S.fromList [(x+k,y) | k <- [0..l]]
        pl (x,y) (DLeft, l) = S.fromList [(x-k,y) | k <- [0..l]]


showD = unlines . P.map show
