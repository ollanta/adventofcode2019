import Prelude as P
import Text.Parsec
import Data.HashMap.Strict as M
import Data.List as L
import Data.Function
import Data.Ord

main :: IO ()
main = do
  interact (showD . solve . readD)
  putStrLn ""


showD = unlines . P.map show . zip [1..]


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


toMap ps = M.fromList withCoords
  where
    width = length $ head ps
    height = length ps
    withCoords = zip [(x,y) | y <- [0..height-1], x <- [0..width-1]] (concat ps)


laserCoord = (19,11)
--laserCoord = (11,13)

solve ps = killAsteroids laserCoord (M.delete laserCoord pmap)
  where
    pmap = toMap ps


killAsteroids :: Coord -> HashMap Coord Point -> [Coord]
killAsteroids k pmap
  | M.null pmap  = []
  | otherwise = orderedAsteroids ++ killAsteroids k pmap'
  where
    asteroids = keys . M.filter (==Asteroid) $ pmap

    pmap' = L.foldr (\c m -> M.delete c m) pmap orderedAsteroids

    directions :: [(Coord,Coord)]
    directions = nubBy ((==) `on` fst) . sortBy (comparing (toDist . snd)) . P.map (\c -> (toDx k c, c)) $ asteroids
    orderedDirections = sortBy (comparing (angle . fst)) directions

    (kx,ky) = k
    toDist (cx,cy) = (cx-kx)*(cx-kx) + (cy-ky)*(cy-ky)

    orderedAsteroids = P.map snd orderedDirections


angle (dx,dy)
  | ang < 0 = 2*pi + ang
  | otherwise = ang
  where
    fx = fromIntegral dx
    fy = fromIntegral dy
    ang = atan2 fx (-fy)


toDx :: Coord -> Coord -> Coord
toDx (kx,ky) (cx,cy) = (dxo,dyo)
  where
    (dx,dy) = (cx-kx, cy-ky)
    g       = gcd dx dy
    (dxo,dyo) = (dx `div` g, dy `div` g)
