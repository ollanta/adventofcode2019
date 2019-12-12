import Text.Parsec
import Data.List as L
import qualified Data.HashMap.Strict as M


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


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

xstate (Moon (x,_,_) (vx,_,_)) = (x,vx)
ystate (Moon (_,y,_) (_,vy,_)) = (y,vy)
zstate (Moon (_,_,z) (_,_,vz)) = (z,vz)


solve :: [Coord] -> Integer
solve positions = maximum starts + foldr1 lcm loopsizes
  where
    initMoons = [Moon{pos=pos, vel=(0,0,0)} |  pos <- positions]

    moonstates = iterate orbitMoons initMoons

    --xmoonstates = map (map xstate) moonstates
    splitstates = [map (map dimstate) moonstates
                  | dimstate <- [xstate, ystate, zstate]]

    (starts, ends) = unzip $ map findRepeats splitstates

    loopsizes = zipWith (-) ends starts


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


findRepeats states = helper 0 M.empty states
  where
    helper i m (s:rest)
      | M.member s m = (m M.! s, i)
      | otherwise    = helper (i+1) (M.insert s i m) rest
