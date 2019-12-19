import Intcode

main :: IO ()
main = do
  s <- getContents
  putStrLn (showD . solve . readProgram $ s)

showD = show

solve :: Program -> (Integer, Integer)
solve program = loop (10,5) 99
  where
    -- seed the loop with a value on or above the (full) tractor beam
    loop c@(x,y) d
      | check (x,y+d) = loop (x+1,y) d
      | check (x+d,y) = loop (x,y+1) d
      | otherwise = c

    check (x,y) = (==0) . head . run $ initComputer program [x,y]
