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


showD = unlines . map show


toProgram ::[Integer] -> M.HashMap Integer Integer
toProgram list = program
  where
    program = M.fromList $ zip [0..] list


solve :: [Integer] -> [[Integer]]
solve list = [run 0 0 [1] program, -- part1
              run 0 0 [2] program] -- part2
  where
    program = toProgram list


data Param = Pos !Integer | Val !Integer


run :: Integer -> Integer -> [Integer] -> M.HashMap Integer Integer -> [Integer]
run i reli rs prog
  | op == 99 = []
  | op == 1  = run (i+4) reli rs padd
  | op == 2  = run (i+4) reli rs pmul
  | op == 3  = run (i+2) reli readRest (pinsert (arg 1) readNext)
  | op == 4  = rarg 1 : run (i+2) reli rs prog
  | op == 5  = run (jumpif True)  reli rs prog
  | op == 6  = run (jumpif False) reli rs prog
  | op == 7  = run (i+4) reli rs pless
  | op == 8  = run (i+4) reli rs pequals
  | op == 9  = run (i+2) (reli + rarg 1) rs prog
  where
    plookup (Pos k) = M.lookupDefault 0 k prog
    plookup (Val v) = v
    pinsert (Pos k) param = M.insert k param prog

    opcode:parameters = map (plookup . Pos) [i..]

    (modecode, op) = opcode `divMod` 100

    modes = parseModecode modecode

    args = [
      case m of
        0 -> Pos param
        1 -> Val param
        2 -> Pos (reli + param)
      | (m, param) <- zip modes parameters
      ]
    arg i = args !! (i-1)
    rarg i = plookup (arg i)

    readNext:readRest = rs

    padd = pinsert (arg 3) (rarg 1 + rarg 2)
    pmul = pinsert (arg 3) (rarg 1 * rarg 2)
    pless   = pinsert (arg 3) (if rarg 1 < rarg 2 then 1 else 0)
    pequals = pinsert (arg 3) (if rarg 1 == rarg 2 then 1 else 0)

    jumpif b
      | b == (rarg 1 > 0) = rarg 2
      | otherwise         = i+3


parseModecode :: Integer -> [Integer]
parseModecode modecode = unfoldr (\m -> Just . flipP $ m `divMod` 10) modecode
  where
    flipP (a,b) = (b,a)
