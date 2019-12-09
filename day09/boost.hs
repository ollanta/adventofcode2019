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
solve list = [run part1,
              run part2]
  where
    part1 = Computer 0 0 [1] program
    part2 = Computer 0 0 [2] program
    program = toProgram list


data Computer = Computer {
  pointer :: Integer,
  relPointer :: Integer,
  inputs :: [Integer],
  program :: M.HashMap Integer Integer
}


data Param = Pos !Integer | Val !Integer


run :: Computer -> [Integer]
run c@Computer{pointer=i, relPointer=reli, inputs=rs, program=prog}
  | op == 99 = []
  | op == 1  = run c{pointer=i+4, program=padd}
  | op == 2  = run c{pointer=i+4, program=pmul}
  | op == 3  = run c{pointer=i+2, inputs=readRest, program=pinsert (arg 1) readNext}
  | op == 4  = rarg 1 : run c{pointer=i+2}
  | op == 5  = run c{pointer=jumpif True}
  | op == 6  = run c{pointer=jumpif False}
  | op == 7  = run c{pointer=i+4, program=pless}
  | op == 8  = run c{pointer=i+4, program=pequals}
  | op == 9  = run c{pointer=i+2, relPointer=reli + rarg 1}
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
