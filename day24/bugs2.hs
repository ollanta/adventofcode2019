import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Helpers
import Data.Maybe
import Data.List


main :: IO ()
main = do
  s <- getContents
  putStrLn (showD . solve . readD $ s)


data Obj = Bug | Open
  deriving (Eq, Show)

type Coord = (Integer, Integer)

readD :: String -> M.HashMap Coord Obj
readD s = M.fromList [((x,y), obj) | (y,objline) <- zip [0..] objLines, (x,obj) <- zip [0..] objline]
  where
    objLines = map (map toObj) (lines s)

toObj '#' = Bug
toObj '.' = Open

showD = show

drawm :: M.HashMap Coord Obj -> String
drawm = drawMap vObj

vObj Bug = '#'
vObj Open = '.'


solve orgmap = count
  where
    minutes = 200
    maxl    = minutes `div` 2
    
    orgmap' = M.fromList . map (\(k,v) -> ((0,k),v)) $ M.toList orgmap
    restmap = M.fromList [((l,(x,y)), Open) | l <- [-maxl..maxl], x <- [0..4], y <- [0..4]]

    omap :: M.HashMap (Integer,Coord) Obj
    omap = M.union orgmap' restmap

    states = iterate updatem omap

    count = M.size . M.filter (==Bug) $ states !! fromInteger minutes


updatem omap = M.mapWithKey deadOrAlive omap
  where
    deadOrAlive (_,(2,2)) v = v -- never change the recursing hex
    deadOrAlive k v
      | v == Open && bugNeighbors == 1 = Bug
      | v == Open && bugNeighbors == 2 = Bug
      | v == Bug  && bugNeighbors /= 1 = Open
      | otherwise                      = v
      where
        bugNeighbors = length . filter (==Bug) . map (\c -> M.lookupDefault Open c omap) $ neighbors k


neighbors (l,(ox,oy)) = orgn''
  where
    orgn  = zip (repeat l) [(ox+1,oy), (ox-1,oy), (ox,oy+1), (ox,oy-1)]
    orgn' = map toOuter orgn
    toOuter k@(l,(x,y))
      | x < 0 = (l-1,(1,2))
      | y < 0 = (l-1,(2,1))
      | x > 4 = (l-1,(3,2))
      | y > 4 = (l-1,(2,3))
      | otherwise = k
    orgn'' = concatMap toInner orgn'
    toInner k@(l,(2,2))
      | ox == 1 = zip (repeat (l+1)) [(0,y') | y' <- [0..4]]
      | ox == 3 = zip (repeat (l+1)) [(4,y') | y' <- [0..4]]
      | oy == 1 = zip (repeat (l+1)) [(x',0) | x' <- [0..4]]
      | oy == 3 = zip (repeat (l+1)) [(x',4) | x' <- [0..4]]
    toInner k = [k]
    
    
