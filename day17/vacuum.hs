import Text.Parsec
import qualified Data.Vector.Unboxed as V
import qualified Data.HashMap.Strict as M
import Helpers
import Intcode
import Data.Char


main :: IO ()
main = do
  s <- getContents
  putStrLn (showD . solve . readProgram $ s)


showD (m, sol) = drawMap id m ++ show sol


solve :: Program -> (M.HashMap (Integer,Integer) Char, Integer)
solve program = (chmap, sum . map (\(a,b) -> a*b) . M.keys $ intersections)
  where
    outs = run $ initComputer program []
    chlist = map (chr.fromInteger) outs

    chmap :: M.HashMap (Integer,Integer) Char
    chmap = M.fromList [((x,y),c) | (y,clin) <- zip [0..] (lines chlist), (x,c) <- zip [0..] clin]
    
    intersections = M.filterWithKey scaffoldNeighbors . M.filter (=='#') $ chmap

    scaffoldNeighbors c _ = all (=='#') [M.lookupDefault '#' (x',y') chmap | (x',y') <- neighbors c]

    neighbors (x,y) = [(x+1,y), (x-1,y), (x,y+1), (x,y-1)]
