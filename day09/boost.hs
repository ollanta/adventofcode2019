import qualified Data.HashMap.Strict as M
import Data.List

main :: IO ()
main = do
  interact (showD . solve . readD)
  putStrLn ""


readD :: String -> [Integer]
readD "" = []
readD s  = read s' : readD s'''
  where
    (s', s'') = span (/=',') s
    s''' = dropWhile (==',') s''


showD = show


toProgram ::[Integer] -> M.HashMap Integer Integer
toProgram list = program
  where
    program = M.fromList $ zip [0..] list


solve :: [Integer] -> [[Integer]]
solve list = [run 0 0 [1] program, -- part1
              run 0 0 [2] program] -- part2
  where
    program = toProgram list


run :: Integer -> Integer -> [Integer] -> M.HashMap Integer Integer -> [Integer]
run i rbase rs p
  | op == 99 = []
  | op == 1  = run (i+4) rbase rs padd
  | op == 2  = run (i+4) rbase rs pmul
  | op == 3  = run (i+2) rbase (drop 1 rs) (M.insert p1 (head rs) p)
  | op == 4  = op1 : run (i+2) rbase rs p
  | op == 5  = run (jumpif True)  rbase rs p
  | op == 6  = run (jumpif False) rbase rs p
  | op == 7  = run (i+4) rbase rs pless
  | op == 8  = run (i+4) rbase rs pequals
  | op == 9  = run (i+2) (rbase+op1) rs p
  where
    plookup k = M.lookupDefault 0 k p
    opcode = plookup i
    op   = opcode `mod` 100
    mode = opcode `div` 100
    (ms,m1) = mode `divMod` 10
    (m3,m2) = ms `divMod` 10

    r1 = plookup (i+1)
    r2 = plookup (i+2)
    r3 = plookup (i+3)

    v1 = plookup r1
    v2 = plookup r2

    vr1 = plookup (rbase + r1)
    vr2 = plookup (rbase + r2)

    p1 = case m1 of
      0 -> r1
      2 -> rbase + r1
    p3 = case m3 of
      0 -> r3
      2 -> rbase + r3
    op1 = case m1 of
      0 -> v1
      1 -> r1
      2 -> vr1
    op2 = case m2 of
      0 -> v2
      1 -> r2
      2 -> vr2


    padd = M.insert p3 (op1+op2) p
    pmul = M.insert p3  (op1*op2) p
    pless   = M.insert p3 (if op1<op2 then 1 else 0) p
    pequals = M.insert p3 (if op1==op2 then 1 else 0) p

    jumpif b
      | b == (op1 > 0) = op2
      | otherwise      = i+3
