import Text.Parsec
import qualified Data.Vector.Unboxed as V
import qualified Data.HashMap.Strict as M
import Helpers
import Intcode
import Data.Char
import System.IO (stdin, hSetBuffering, BufferMode(NoBuffering), hSetEcho)
import System.IO.Unsafe (unsafePerformIO)


main :: IO ()
main = do
  prg <- readFile "input.txt"

  inputs <- getContents

  putStr $ solve (readProgram prg) inputs


solve :: Program -> String -> String
solve program inputs = map (chr . fromInteger) outs
  where
    initC = initComputer program

    outs = loop inputs [initC] (runMulti initC [])

    loop inputs lcs (o,Just c) = o ++ cont inp
      where
        inp = takeWhile (/=chr 10) inputs
        inp' = inp ++ [chr 10]
        rest = drop (length inp') inputs

        lc:lcr = lcs
        cont "bk" = loop rest lcr (map (toInteger . ord) "Rewinding...\n", Just lc)
        cont _ = loop rest (c:lcs) oc'

        oc' = runMulti c $ map (toInteger . ord) inp'
    loop inputs (lc:lcs) (o,Nothing) = loop inputs lcs (o,Just lc)
