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

showD = unlines

drawm = drawMap cObj

cObj Bug = '#'
cObj Open = '.'


biodiv omap = sum . M.elems . M.mapWithKey (\(x,y) v -> 2^(y*5+x)) . M.filter (==Bug) $ omap

solve omap = ["Solution"
             ,drawm repstate
             ,show $ biodiv repstate
             ]
             -- ++ map drawm . take 5 . iterate updatem $ omap
  where
    states = iterate updatem omap

    repstate = M.map toObj . findDupe S.empty $ map (M.map cObj) states

    findDupe seen (s:sts)
      | S.member s seen = s
      | otherwise       = findDupe (S.insert s seen) sts


updatem omap = M.mapWithKey deadOrAlive omap
  where
    deadOrAlive k v
      | v == Open && bugNeighbors == 1 = Bug
      | v == Open && bugNeighbors == 2 = Bug
      | v == Bug  && bugNeighbors /= 1 = Open
      | otherwise                      = v
      where
        bugNeighbors = length . filter (==Bug) . map (\c -> M.lookupDefault Open c omap) $ neighbors k


neighbors (x,y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
