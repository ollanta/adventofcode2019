import Text.Parsec
import Data.List as L
import Data.HashSet as S
import Data.Ord


main :: IO ()
main = do
  interact (showD . solve . readD)
  putStrLn ""


showD states = unlines . take 1001 $ shownStates
  where
    shownStates = zipWith showiState [0..] states
    showiState i state = unlines ["Step " ++ show i, unlines (L.map show state), show (energy state)]


energy moons = sum (map energy1 moons)
  where
    energy1 (Moon (a,b,c) (d,e,f)) = sum (map abs [a,b,c]) * sum (map abs [d,e,f])


type Coord = (Int, Int, Int)

addc (x1,y1,z1) (x2,y2,z2) = (x1+x2,y1+y2,z1+z2)


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


solve positions = moonstates
  where
    moons = [Moon{pos=pos, vel=(0,0,0)} |  pos <- positions]

    moonstates = iterate updateState moons

    updateState allMoons = map (updateOneState allMoons) allMoons

    updateOneState allMoons moon = Moon{pos=addc (pos moon) vel', vel=vel'}
      where
        otherMoons = filter (/=moon) allMoons
        gravities = map (gravity moon) otherMoons
        gravity Moon{pos=(x1,y1,z1)} Moon{pos=(x2,y2,z2)} = (gravity1 x1 x2, gravity1 y1 y2, gravity1 z1 z2)
        gravity1 c1 c2
          | c1 == c2 = 0
          | c1 <  c2 = 1
          | c1 >  c2 = -1
        sumgravities = foldr1 addc gravities
        vel' = addc (vel moon) sumgravities
