import Text.Parsec
import Data.HashMap.Strict as M
import Data.HashSet as S
import Data.Ord
import Data.List


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


toPmap :: [[Point]] -> HashMap Coord Point
toPmap ps = M.fromList withCoords
  where
    width = length $ head ps
    height = length ps
    withCoords = zip [(x,y) | y <- [0..height-1], x <- [0..width-1]] (concat ps)


solve ps = maximumBy (comparing snd) . M.toList $ mapInview pmap
  where
    pmap = toPmap ps


mapInview :: HashMap Coord Point -> HashMap Coord Int
mapInview pmap = mapWithKey inview pmap
  where
    asteroids = keysSet . M.filter (==Asteroid) $ pmap

    inview k Free     = 0
    inview k Asteroid = inviewFrom k (S.delete k asteroids)

    inviewFrom k coords = S.size $ S.map (toDx k) coords


toDx (kx,ky) (cx,cy) = (dx `div` g, dy `div` g)
  where
    (dx,dy) = (cx-kx, cy-ky)
    g       = gcd dx dy
