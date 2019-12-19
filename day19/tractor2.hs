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


--showD (m, sol) = drawMap toVis m ++ show sol
showD = show

toVis 1 = '#'
toVis 0 = '.'

--solve :: Program -> (M.HashMap (Integer,Integer) Integer, Int)
solve program = result
  where
    -- seed the loop with a value on or above the (full) tractor beam
    result = loop (100,60) 99

    loop c@(x,y) d
      | not (check c)  = loop (x,y+1) d
      | not (check (x,y+d)) = loop (x+1,y) d
      | not (check (x+d,y)) = loop (x,y+1) d
      | otherwise = c

    check (x,y) = (==1) . head . run $ initComputer program [x,y]
