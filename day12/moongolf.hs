
initPos=[[-7,-8,9],[-12,-3,-4],[6,-17,-9],[4,-10,-6]]

main :: IO ()
main = putStrLn . show . energy $ solve initPos

energy moons = sum (map energy1 moons)
  where
    energy1 (pos, vel) = sum (map abs pos) * sum (map abs vel)

type Moon = ([Integer],[Integer])

solve :: [[Integer]] -> [Moon]
solve positions = (!!1000) $ iterate orbitMoons [(pos, [0,0,0]) |  pos <- positions]

orbitMoons :: [Moon] -> [Moon]
orbitMoons moons = map orbitMoon moons
  where
    orbitMoon moon@(p,v) = (zipWith (+) p v', v')
      where
        gravities = map (gravity moon) moons
        sumgravities = foldr1 (zipWith (+)) gravities
        v' = zipWith (+) v sumgravities

gravity :: Moon -> Moon -> [Integer]
gravity (p1,_) (p2,_) = map gravity1 $ zipWith (-) p2 p1
  where
    gravity1 0 = 0
    gravity1 d = signum d
