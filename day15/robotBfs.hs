import Text.Parsec
import Data.List as L
import qualified Data.HashMap.Strict as M
import Data.Ord
import Data.Function
import Intcode
import Helpers

main :: IO ()
main = do
  s <- readFile "input.txt"
  putStrLn (showD . solve . readProgram $ s)

showD = unlines

solve program = [show i, draw finalMap]
  where
    (Just i, finalMap) = bfs 0 initMap [(initCoord, initComputer program)] []

    initCoord = (0,0)
    initMap = M.singleton initCoord Open

    bfs i m [] []   = (Nothing, m)
    bfs i m [] next = bfs (i+1) m next []
    bfs i m ((c, comp):ls) next = if m M.! c == Oxygen then (Just i, m) else bfs i m' ls (next++new)
      where
        unexploredNeighbors = filter (not . (`M.member` m)) $ allNeighbors c
        (outputs, comps) = unzip [(out, c') | n <- unexploredNeighbors, let ([out], Just c') = runOne comp (moveTo c n)]
        newObjs = map toObj outputs
        m' = M.union m . M.fromList $ zip unexploredNeighbors newObjs
        new = [(c,comp) | (c, comp, obj) <- zip3 unexploredNeighbors comps newObjs, obj /= Wall]

allNeighbors c = [addc c dc | dc <- [(0,1),(1,0),(0,-1),(-1,0)]]

toObj 0 = Wall
toObj 1 = Open
toObj 2 = Oxygen

moveTo oc@(ox,oy) nc@(nx,ny)
  | nx == ox+1 = 4
  | nx == ox-1 = 3
  | ny == oy-1 = 2
  | ny == oy+1 = 1


type Coord = (Integer, Integer)

addc (x1,y1) (x2,y2) = (x1+x2,y1+y2)

data Obj = Open | Wall | Oxygen | Bot | Origin
  deriving (Eq)

draw :: M.HashMap Coord Obj -> String
draw objmap = drawMap draw1 objmap'
  where
    objmap' = M.insert (0,0) Origin objmap

    draw1 Open = '.'
    draw1 Wall = '#'
    draw1 Oxygen = 'o'
    draw1 Bot = 'B'
    draw1 Origin = 'X'
