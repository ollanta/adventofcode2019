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
solve program = contOutput outs2
  where
    program' = M.insert 0 2 program

    inputs = ["A,A,C,C,B,C,B,C,B,A",
              "L,10,L,10,R,6",
              "L,6,L,10,R,12,R,12",
              "R,12,L,12,L,12",
              "y"
             ]

    outs2 = run $ initComputer program' (map (toInteger.ord) . unlines $ inputs)


contOutput [a] = [show a]
contOutput l = first' : contOutput rest'
  where
    (first,rest) = span (/=10) l
    first' = map (chr . fromInteger) first
    rest' = drop 1 rest
