import Prelude as P
import Text.Parsec
import Data.HashMap.Strict as M
import Data.List as L
import Data.Ord


main :: IO ()
main = do
  interact (showD . solve . readD)
  putStrLn ""


showD = show


type Coord = (Int, Int)


data Point = Asteroid | Free
  deriving (Show, Eq)

readD :: String -> [[Point]]
readD s = points
  where
    Right points = parse ((`endBy` newline) $ many1 readP ) "" s
    readP = readC <$> oneOf ".#"

    readC '.' = Free
    readC '#' = Asteroid


toMap :: [[Point]] -> HashMap Coord Point
toMap ps = M.fromList withCoords
  where
    width = length $ head ps
    height = length ps
    withCoords = zip [(x,y) | y <- [0..height-1], x <- [0..width-1]] (concat ps)


solve ps = maximumBy (comparing snd) . toList $ mapWithKey insight pmap
  where
    pmap = toMap ps

    asteroids = keys . M.filter (==Asteroid) $ pmap

    insight k Free = 0
    insight k Asteroid = length . insightFrom k $ asteroids

    insightFrom k coords = nub . P.map (toDx k) $ P.filter (/=k) coords

toDx (kx,ky) (cx,cy) = (dxo,dyo)
  where
    (dx,dy) = (cx-kx, cy-ky)
    g       = gcd dx dy
    (dxo,dyo) = (dx `div` g, dy `div` g)
