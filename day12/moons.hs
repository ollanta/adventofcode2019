import Text.Parsec
import Data.List as L


main :: IO ()
main = do
  interact (showD . solve . readD)
  putStrLn ""


showD states = unlines . take 1001 $ shownStates
  where
    shownStates = zipWith showiState [0..] states
    showiState i state = unlines ["Step " ++ show i,
                                  unlines (map show state),
                                  "Energy: " ++ show (energy state)]


energy moons = sum (map energy1 moons)
  where
    energy1 (Moon (a,b,c) (d,e,f)) = sum (map abs [a,b,c]) * sum (map abs [d,e,f])


type Coord = (Int, Int, Int)

addc = pairwise (+)

pairwise f (x1,y1,z1) (x2,y2,z2) = (f x1 x2, f y1 y2, f z1 z2)


readD :: String -> [Coord]
readD s = moons
  where
    Right moons = parse (readM `endBy` newline) "" s


readM = do
  string "<x="
  x <- many (noneOf [','])
  string ", y="
  y <- many (noneOf [','])
  string ", z="
  z <- many (noneOf ['>'])
  char '>'
  return (read x, read y, read z)


data Moon = Moon{pos:: Coord, vel::Coord}
  deriving (Eq, Show)


solve :: [Coord] -> [[Moon]]
solve positions = moonstates
  where
    initMoons = [Moon{pos=pos, vel=(0,0,0)} |  pos <- positions]

    moonstates = iterate orbitMoons initMoons


orbitMoons :: [Moon] -> [Moon]
orbitMoons moons = map orbitMoon moons
  where
    orbitMoon moon@Moon{pos=mpos, vel=mvel} = Moon{pos=addc mpos vel', vel=vel'}
      where
        gravities = map (gravity moon) moons
        sumgravities = foldr1 addc gravities
        vel' = addc mvel sumgravities


gravity :: Moon -> Moon -> Coord
gravity Moon{pos=thisPos} Moon{pos=otherPos} = pairwise gravity1 thisPos otherPos
  where
    gravity1 c1 c2
      | c1 == c2 = 0
      | c1 <  c2 = 1
      | c1 >  c2 = -1
