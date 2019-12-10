import Text.Parsec
import Data.HashMap.Strict as M
import Data.HashSet as S
import Data.List as L
import Data.Ord


main :: IO ()
main = do
  interact (showD . solve . readD)
  putStrLn ""


showD = unlines . L.map show . zip [1..]


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
toPmap allPs = M.fromList withCoords
  where
    withCoords = [((x,y), point) |
                  (y,row) <- zip [0..] allPs,
                  (x,point) <- zip [0..] row]


solve ps = killAsteroids laserCoord (S.delete laserCoord asteroids)
  where
    pmap = toPmap ps
    (laserCoord, _) = maximumBy (comparing snd) . M.toList $ mapInview pmap
    asteroids = keysSet . M.filter (==Asteroid) $ pmap


killAsteroids :: Coord -> HashSet Coord -> [Coord]
killAsteroids k asteroids
  | S.null asteroids = []
  | otherwise        = orderedAsteroids ++ killAsteroids k asteroids'
  where
    asteroids' = S.difference asteroids (S.fromList orderedAsteroids)

    byDirection :: HashMap Coord Coord
    byDirection = M.fromListWith chooseC . L.map (\c -> (toDx k c, c)) . S.toList $ asteroids

    chooseC a b
      | dist a k < dist b k = a
      | otherwise           = b

    orderedDirections = sortBy (comparing (angle . fst)) . M.toList $ byDirection

    orderedAsteroids = L.map snd orderedDirections


dist :: Coord -> Coord -> Int
dist (ax,ay) (bx,by) = (ax-bx)^2 + (ay-by)^2


angle :: Coord -> Double
angle (dx,dy)
  | ang < 0 = 2*pi + ang
  | otherwise = ang
  where
    fx = fromIntegral dx
    fy = fromIntegral dy
    ang = atan2 fx (-fy)


mapInview :: HashMap Coord Point -> HashMap Coord Int
mapInview pmap = mapWithKey inview pmap
  where
    asteroids = keysSet . M.filter (==Asteroid) $ pmap

    inview k Free     = 0
    inview k Asteroid = inviewFrom k (S.delete k asteroids)

    inviewFrom k coords = S.size $ S.map (toDx k) coords


toDx :: Coord -> Coord -> Coord
toDx (kx,ky) (cx,cy) = (dxo,dyo)
  where
    (dx,dy) = (cx-kx, cy-ky)
    g       = gcd dx dy
    (dxo,dyo) = (dx `div` g, dy `div` g)
