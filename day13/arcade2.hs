import qualified Data.HashMap.Strict as M
import Data.List
import Intcode
import Helpers


main :: IO ()
main = do
  interact (showD . solve . readProgram)
  putStrLn ""


showD = unlines


type Coord = (Integer, Integer)


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


solve :: Program -> [String]
solve program = lastN 5 $ map draw interesting
  where
    hasPaddle = not . M.null . M.filter (==3)
    hasBall = not . M.null . M.filter (==4)
    getPaddle = head . M.keys . M.filter (==3)
    getBall = head . M.keys . M.filter (==4)

    interesting = filter (\m -> hasPaddle m && hasBall m) maps
    ps = map getPaddle interesting
    bs = map getBall interesting

    inputs = 0:botinp ps bs

    outputs = run (initComputer program) inputs

    (xs, ys, ts) = split outputs
    instrs = zip (zip xs ys) ts

    maps = scanl (\m (c,t) -> M.insert c t m) M.empty instrs


botinp (p1:restp@(p2:ps)) (b1:restb@(b2:bs))
  | b1 == b2  = cont
  | px <  b2x = 1 : cont
  | px >  b2x = -1 : cont
  | otherwise = 0 : cont
  where
    cont = botinp restp restb
    (px,_) = p1
    (b2x,_) = b2
botinp _ _ = []
