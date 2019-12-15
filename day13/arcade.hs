import qualified Data.HashMap.Strict as M
import Data.List
import Intcode

main :: IO ()
main = do
  interact (show . solve . readProgram)
  putStrLn ""


split (x1:x2:x3:xs) = let (xr1,xr2,xr3) = split xs in (x1:xr1, x2:xr2, x3:xr3)
split [] = ([],[],[])


solve :: Program -> Int
solve program = M.size $ M.filter (==2) finalmap
  where
    outputs = run $ initComputer program []

    (xs, ys, ts) = split outputs
    instrs = zip (zip xs ys) ts

    finalmap = foldl (\m (c,t) -> M.insert c t m) M.empty instrs
