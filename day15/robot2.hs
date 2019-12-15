import Text.Parsec
import Data.List as L
import qualified Data.HashMap.Strict as M
import Data.Ord
import Data.Function

main :: IO ()
main = do
  s <- readFile "input.txt"
  putStrLn (showD . solve $ readD s)


showD = unlines


solve list = [show  . length $ filled,
              draw . last $ filled]
  where
    filling = iterate fillWithOxygen finalMap
    filled = takeWhile (not . M.null . M.filter (==Open)) filling
    
    [oxygenPos] = M.keys $ M.filter (==Oxygen) finalMap

    findFinal 5 (l:ls)     = l
    findFinal n (l@(c,_,_):ls)
      | c == (0,0) = findFinal (n+1) ls
      | otherwise  = findFinal n ls
    (_, finalMap, finalSP) = findFinal 0 (zip3 coords maps smallPaths)

    program = toProgram list

    initCoord = (0,0)
    initMap = M.fromList [(initCoord,Open)]

    outputs = run (Computer 0 0 inputs program)

    moves = zipWith toMove inputs outputs

    coords = scanl addc initCoord moves

    newinfo = zipWith3 toInfo coords inputs outputs

    maps = scanl (\m (c,o) -> M.insert c o m) initMap newinfo
    mapsP = zipWith (\c m -> M.insert c Bot m) coords maps

    initSmallPath = M.fromList [(initCoord,[])]
    smallPaths = scanl (\m (inp,oc,c) -> updateSmallPath m inp oc c) initSmallPath (zip3 inputs coords (drop 1 coords))

    inputs :: [Integer]
    inputs = zipWith3 getNext maps coords smallPaths


fillWithOxygen m = m'
  where
    oxypos = M.keys $ M.filter (==Oxygen) m
    oxyneighbors = concatMap allNeighbors oxypos

    m' = foldl (\m c -> M.adjust fill c m) m oxyneighbors
    fill Open = Oxygen
    fill o    = o


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


readD :: String -> [Integer]
readD "" = []
readD s  = read s' : readD s'''
  where
    (s', s'') = span (/=',') s
    s''' = dropWhile (==',') s''


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


toProgram ::[Integer] -> M.HashMap Integer Integer
toProgram list = program
  where
    program = M.fromList $ zip [0..] list


data Computer = Computer {
  pointer :: Integer,
  relPointer :: Integer,
  inputs :: [Integer],
  program :: M.HashMap Integer Integer
}


data Param = Pos !Integer | Val !Integer


run :: Computer -> [Integer]
run c@Computer{pointer=i, relPointer=reli, inputs=rs, program=prog}
  | op == 99 = []
  | op == 1  = run c{pointer=i+4, program=padd}
  | op == 2  = run c{pointer=i+4, program=pmul}
  | op == 3  = run c{pointer=i+2, inputs=readRest, program=pinsert (arg 1) readNext}
  | op == 4  = rarg 1 : run c{pointer=i+2}
  | op == 5  = run c{pointer=jumpif True}
  | op == 6  = run c{pointer=jumpif False}
  | op == 7  = run c{pointer=i+4, program=pless}
  | op == 8  = run c{pointer=i+4, program=pequals}
  | op == 9  = run c{pointer=i+2, relPointer=reli + rarg 1}
  where
    plookup (Pos k) = M.lookupDefault 0 k prog
    plookup (Val v) = v
    pinsert (Pos k) param = M.insert k param prog

    opcode:parameters = map (plookup . Pos) [i..]

    (modecode, op) = opcode `divMod` 100

    modes = parseModecode modecode

    args = [
      case m of
        0 -> Pos param
        1 -> Val param
        2 -> Pos (reli + param)
      | (m, param) <- zip modes parameters
      ]
    arg i = args !! (i-1)
    rarg i = plookup (arg i)

    readNext:readRest = rs

    padd = pinsert (arg 3) (rarg 1 + rarg 2)
    pmul = pinsert (arg 3) (rarg 1 * rarg 2)
    pless   = pinsert (arg 3) (if rarg 1 < rarg 2 then 1 else 0)
    pequals = pinsert (arg 3) (if rarg 1 == rarg 2 then 1 else 0)

    jumpif b
      | b == (rarg 1 > 0) = rarg 2
      | otherwise         = i+3


parseModecode :: Integer -> [Integer]
parseModecode modecode = unfoldr (\m -> Just . flipP $ m `divMod` 10) modecode
  where
    flipP (a,b) = (b,a)
