import qualified Data.HashMap.Strict as M
import Helpers
import Intcode

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
    coords = [(x,y) | x <- [0..49], y <- [0..49]]
    outs = concat [run $ initComputer program [x,y] | (x,y) <- coords]
    chlist = zip coords outs

    chmap :: M.HashMap (Integer,Integer) Integer
    chmap = M.fromList chlist
