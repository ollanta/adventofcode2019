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

showD = unlines

solve :: Program -> [String]
solve program = contOutput outs
  where

    inputs = ["OR A T",
              "AND B T",
              "AND C T", -- T = all(A,B,C)
              "NOT T J", -- J = !all(A,B,C)
              "AND D J", -- J = D & !all(A,B,C) -- should jump soon
              "NOT E T", -- T = !E
              "AND H T", -- T = !E & H
              "OR E T",  -- T = E || (!E & H) -- we can jump again immediately if need be
              "AND T J", -- J = should jump soon & we can jump again if need be
              "RUN"
             ]

    outs = run (initComputer program) (map (toInteger.ord) . unlines $ inputs)



contOutput [a] = [show a]
contOutput l = first' : contOutput rest'
  where
    (first,rest) = span (/=10) l
    first' = map (chr . fromInteger) first
    rest' = drop 1 rest
