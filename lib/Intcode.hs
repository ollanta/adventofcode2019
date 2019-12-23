module Intcode
  ( Computer
  , Program
  , initComputer
  , run
  , runOne
  , runMulti
  , readProgram
  ) where

import qualified Data.HashMap.Strict as M
import qualified Data.List as L


data Computer = Computer {
  pointer :: Integer,
  relPointer :: Integer,
  program :: Program
}


data Param = Pos !Integer | Val !Integer


type Program = M.HashMap Integer Integer


readProgram :: String -> Program
readProgram = toProgram . readProglist


readProglist :: String -> [Integer]
readProglist "" = []
readProglist s  = read s' : readProglist s'''
      where
        (s', s'') = span (/=',') s
        s''' = dropWhile (==',') s''


toProgram :: [Integer] -> M.HashMap Integer Integer
toProgram list = M.fromList $ zip [0..] list


initComputer :: Program -> Computer
initComputer program = Computer 0 0 program


run :: Computer -> [Integer] -> [Integer]
run c inputs = run' started inputs
  where
    started = runCont c Nothing -- start, in case no input is needed

    run' :: CompCont -> [Integer] -> [Integer]
    run' Halted _ = []
    run' (Output i cc) is  = i:run' cc is
    run' (Paused c) (i:is) = run' (runCont c (Just i)) is


data CompCont = Output Integer CompCont | Paused Computer | Halted


runOne :: Computer -> Integer -> ([Integer], Maybe Computer)
runOne c i = runOne' (runCont c (Just i)) []
  where
    runOne' :: CompCont -> [Integer] -> ([Integer], Maybe Computer)
    runOne' Halted acc     = (reverse acc, Nothing)
    runOne' (Paused c) acc = (reverse acc, Just c)
    runOne' (Output i cc) acc = runOne' cc (i:acc)


runMulti :: Computer -> [Integer] -> ([Integer], Maybe Computer)
runMulti c inputs = L.foldl helper ([], Just c) inputs
  where
    helper (outputs, Nothing) _ = (outputs, Nothing)
    helper (outputs, Just c)  i = (outputs ++ newout, c')
      where
        (newout, c') = runOne c i


runCont :: Computer -> Maybe Integer -> CompCont
runCont c@Computer{pointer=i, relPointer=reli, program=prog} input
  | op == 99 = Halted
  | op == 1  = runNext c{pointer=i+4, program=padd}
  | op == 2  = runNext c{pointer=i+4, program=pmul}
  | op == 3  = case input of
      Just input -> runCont c{pointer=i+2, program=pinsert (arg 1) input} Nothing
      Nothing  -> Paused c
  | op == 4  = Output (rarg 1) (runNext c{pointer=i+2})
  | op == 5  = runNext c{pointer=jumpif True}
  | op == 6  = runNext c{pointer=jumpif False}
  | op == 7  = runNext c{pointer=i+4, program=pless}
  | op == 8  = runNext c{pointer=i+4, program=pequals}
  | op == 9  = runNext c{pointer=i+2, relPointer=reli + rarg 1}
  where
    runNext c = runCont c input

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

    padd = pinsert (arg 3) (rarg 1 + rarg 2)
    pmul = pinsert (arg 3) (rarg 1 * rarg 2)
    pless   = pinsert (arg 3) (if rarg 1 < rarg 2 then 1 else 0)
    pequals = pinsert (arg 3) (if rarg 1 == rarg 2 then 1 else 0)

    jumpif b
      | b == (rarg 1 > 0) = rarg 2
      | otherwise         = i+3


parseModecode :: Integer -> [Integer]
parseModecode modecode = L.unfoldr (\m -> Just . flipP $ m `divMod` 10) modecode
  where
    flipP (a,b) = (b,a)
