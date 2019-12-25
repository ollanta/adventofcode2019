
import qualified Data.HashMap.Strict as M
import Data.List
import Intcode
import Helpers
import System.IO (stdin, hSetBuffering, BufferMode(NoBuffering), hSetEcho)


main :: IO ()
main = do
  prg <- readFile "input2.txt"
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  inputs <- getInputs
  let maps = solve (readProgram prg) inputs

  putStrLn (showD maps)
  putStrLn ""


showD = unlines


type Coord = (Integer,Integer)


draw :: M.HashMap Coord Integer -> String
draw objmap = unlines ["Score: " ++ show score,
                       drawMap draw objmap']
  where
    score = (M.lookupDefault 0 (-1,0) objmap)
    objmap' = M.delete (-1,0) objmap

    draw 0 = ' '
    draw 1 = 'W'
    draw 2 = 'B'
    draw 3 = '='
    draw 4 = 'o'


split (x1:x2:x3:xs) = let (xr1,xr2,xr3) = split xs in (x1:xr1, x2:xr2, x3:xr3)
split [] = ([],[],[])


solve :: Program -> [Integer] -> [String]
solve program inputs = map draw (drop 1 maps)
  where
    outputs = run (initComputer program) inputs
    (xs, ys, ts) = split outputs
    instrs = zip (zip xs ys) ts
    maps = scanl (\m (c,t) -> M.insert c t m) M.empty instrs


getInputs = getContents >>= return . map toInput
  where
    toInput :: Char -> Integer
    toInput 'a' = -1
    toInput 'd' = 1
    toInput _   = 0
