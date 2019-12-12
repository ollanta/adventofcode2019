import Text.Parsec
import Data.List as L
import Data.HashSet as S
import Data.HashMap.Strict as M
import Data.Ord


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""




energy moons = sum (L.map energy1 moons)
  where
    energy1 (Moon (a,b,c) (d,e,f)) = sum (L.map abs [a,b,c]) * sum (L.map abs [d,e,f])


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

takeWhileIncreasing (a:rest@(b:_))
  | b > a = a : rest
  | otherwise = [a]


--solve :: [Coord] -> Int
solve positions = lcm (lcm (xloopend-xloopstart) (yloopend-yloopstart)) (zloopend-zloopstart)
  where
    xstate (Moon (x,_,_) (vx,_,_)) = (x,vx)
    ystate (Moon (_,y,_) (_,vy,_)) = (y,vy)
    zstate (Moon (_,_,z) (_,_,vz)) = (z,vz)

    xmoonstates = [(as,bs,cs,ds) | l <- moonstates, let (as:bs:cs:ds:_) = L.map xstate l]
    ymoonstates = [(as,bs,cs,ds) | l <- moonstates, let (as:bs:cs:ds:_) = L.map ystate l]
    zmoonstates = [(as,bs,cs,ds) | l <- moonstates, let (as:bs:cs:ds:_) = L.map zstate l]

    (xloopstart, xloopend) = findRepeats xmoonstates
    (yloopstart, yloopend) = findRepeats ymoonstates
    (zloopstart, zloopend) = findRepeats zmoonstates

    initmoons = [Moon{pos=pos, vel=(0,0,0)} |  pos <- positions]

    moonstates = iterate updateState initmoons

    updateState allMoons = L.map (updateOneState allMoons) allMoons

    updateOneState allMoons moon = Moon{pos=addc (pos moon) vel', vel=vel'}
      where
        otherMoons = L.filter (/=moon) allMoons
        gravities = L.map (gravity moon) otherMoons
        gravity Moon{pos=(x1,y1,z1)} Moon{pos=(x2,y2,z2)} = (gravity1 x1 x2, gravity1 y1 y2, gravity1 z1 z2)
        gravity1 c1 c2
          | c1 == c2 = 0
          | c1 <  c2 = 1
          | c1 >  c2 = -1
        sumgravities = foldr1 addc gravities
        vel' = addc (vel moon) sumgravities


findRepeats ps = helper 0 M.empty ps
  where
    helper i m (p:ps)
      | M.member p m = (m M.! p, i)
      | otherwise    = helper (i+1) (M.insert p i m) ps
