
import qualified Data.HashMap.Strict as M
import Data.List
import Intcode
import Helpers

main :: IO ()
main = do
  inp <- getContents
  putStrLn (solve . readProgram $ inp)


type Coord = (Int,Int)


split (x1:x2:xs) = let (xr1,xr2) = split xs in (x1:xr1, x2:xr2)
split [] = ([],[])
  

turn :: Coord -> Integer -> Coord
turn (x,y) 0 = (-y,x)
turn (x,y) 1 = (y,-x)


move :: Coord -> Coord -> Coord
move (x,y) (dx,dy) = (x+dx, y+dy)


draw pmap = unlines . reverse . lines $ drawn
  where
    drawn = drawMap draw pmap
    draw 1 = '#'
    draw 0 = ' '


solve :: Program -> String
solve program = draw (last maps)
  where
    outputs = run (initComputer program) inputs

    (paints, turns) = split outputs 
    directions = drop 1 $ scanl turn (0,1) turns

    coords = scanl move (0,0) directions

    instructions = zip coords paints

    initMap = M.fromList [((0,0),1)]
    maps = scanl (\m (c,p) -> M.insert c p m) initMap instructions

    inputs = zipWith (M.lookupDefault 0) coords maps
