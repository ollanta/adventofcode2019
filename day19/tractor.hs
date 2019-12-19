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


showD (m, sol) = drawMap toVis m ++ show sol

toVis 1 = '#'
toVis 0 = '.'

solve :: Program -> (M.HashMap (Integer,Integer) Integer, Int)
solve program = (chmap, M.size $ M.filter(==1) chmap)
  where
    minX = 0
    minY = 0
    maxX = minX+49
    maxY = minY+49
    coords = [(x,y) | x <- [minX..maxX], y <- [minY..maxY]]
    outs = concat [run $ initComputer program [x,y] | (x,y) <- coords]
    chlist = zip coords outs

    chmap :: M.HashMap (Integer,Integer) Integer
    chmap = M.fromList chlist
