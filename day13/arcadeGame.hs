
import qualified Data.HashMap.Strict as M
import Data.List
import Intcode
import Helpers
import System.IO (stdin, hSetBuffering, BufferMode(NoBuffering), hSetEcho)
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
  inp <- readFile "input2.txt"
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  maps <- solve (readProgram inp)

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


solve :: Program -> IO [String]
solve program = do

  let outputs = run $ initComputer program getInputs
  let (xs, ys, ts) = split outputs
  let instrs = zip (zip xs ys) ts
  let maps = scanl (\m (c,t) -> M.insert c t m) M.empty instrs

  return $ map draw (drop 1 maps)


getInputs = map (\i -> unsafePerformIO $ getOneKey i) [0..]
  where
    getOneKey :: Integer -> IO Integer
    getOneKey i = do
        key <- getChar
        case key of 
            'a' -> return (-1)
            'd' -> return 1
            _ -> return 0
