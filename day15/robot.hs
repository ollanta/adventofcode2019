import Text.Parsec
import Data.List as L
import qualified Data.HashMap.Strict as M
import Data.Ord
import Data.Function
import Intcode

main :: IO ()
main = do
  s <- readFile "input.txt"
  putStrLn (showD . solve . readProgram $ s)


showD = unlines


solve program = [show . length $ finalSP M.! oxygenPos,
                 draw finalMap]
  where
    [oxygenPos] = M.keys $ M.filter (==Oxygen) finalMap

    findFinal 5 (l:ls)     = l
    findFinal n (l@(c,_,_):ls)
      | c == (0,0) = findFinal (n+1) ls
      | otherwise  = findFinal n ls
    (_, finalMap, finalSP) = findFinal 0 (zip3 coords maps smallPaths)

    initCoord = (0,0)
    initMap = M.singleton initCoord Open

    outputs = run $ initComputer program inputs

    moves = zipWith toMove inputs outputs

    coords = scanl addc initCoord moves

    newinfo = zipWith3 toInfo coords inputs outputs

    maps = scanl (\m (c,o) -> M.insert c o m) initMap newinfo

    initSmallPath = M.singleton initCoord []
    smallPaths = scanl (\m (inp,oc,c) -> updateSmallPath m inp oc c) initSmallPath (zip3 inputs coords (drop 1 coords))

    inputs :: [Integer]
    inputs = zipWith3 getNext maps coords smallPaths



getNext :: M.HashMap Coord Obj -> Coord -> M.HashMap Coord [Integer] -> Integer
getNext m c pt
  | null nextOpen = moveTo c smallc
  | otherwise     = moveTo c (head nextOpen)
  where
    nextOpen = [c' | c' <- allNeighbors c, M.member c' m == False]
    (smallc, smallpath) = head $ sortBy (comparing length `on` snd) [(c', pt M.! c') | c' <- allNeighbors c, M.member c' pt]


updateSmallPath m inp oc c
  | oc == c = m
  | c  == (0,0) = m
  | otherwise = M.insert c newp m
  where
    (smallc, smallpath) = head $ sortBy (comparing length `on` snd) [(c', m M.! c') | c' <- allNeighbors c, M.member c' m]
    newp = (moveTo smallc c):smallpath


allNeighbors c = [addc c (toDir dir) | dir <- [1,4,2,3]]


moveTo oc@(ox,oy) nc@(nx,ny)
  | nx == ox+1 = 4
  | nx == ox-1 = 3
  | ny == oy-1 = 2
  | ny == oy+1 = 1


toInfo c i o
  | o == 0 = (c', Wall)
  | o == 1 = (c', Open)
  | o == 2 = (c', Oxygen)
  where
    c' = addc c (toDir i)

toMove _ 0 = (0,0)
toMove d _ = toDir d

toDir 1 = (0,1)
toDir 2 = (0,-1)
toDir 3 = (-1,0)
toDir 4 = (1,0)


type Coord = (Integer, Integer)

addc (x1,y1) (x2,y2) = (x1+x2,y1+y2)

data Obj = Open | Wall | Oxygen | Bot | Unknown | Origin
  deriving (Eq)


draw :: M.HashMap Coord Obj -> String
draw pmap = unlines showMap
  where
    sx = minimum . map fst . M.keys $ pmap
    ex = maximum . map fst . M.keys $ pmap
    sy = minimum . map snd . M.keys $ pmap
    ey = maximum . map snd . M.keys $ pmap
    draw Open = '.'
    draw Wall = '#'
    draw Oxygen = 'o'
    draw Bot = 'B'
    draw Unknown = ' '
    draw Origin = 'X'

    pmap' = M.insert  (0,0) Origin pmap

    showMap = [[draw (M.lookupDefault Unknown (x,y) pmap') | x <- [sx..ex]] | y <- [sy..ey]]
