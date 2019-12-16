import Text.Parsec
import Data.List as L
import qualified Data.HashMap.Strict as M
import Helpers


main :: IO ()
main = do
  s <- getContents
  p <- either (error.show) return (readD s)
  putStrLn (showD $ solve p)


readD :: String -> Either ParseError [Integer]
readD s = parse (many1 readC) "" s
  where
    readC = do
      d <- digit
      return (read [d])


showD = unlines . map (concatMap show)


solve :: [Integer] -> [[Integer]]
solve inps = [take 8 $ sumDigs g finalInput | g <- [0,1,100]]
  where
    -- msg offset is so large we don't need the patterns, it's just sums all the way down
    msgoffset = read . concatMap show . take 7 $ inps
    totlength = 10000 * length inps

    finalInput = drop msgoffset . take totlength $ cycle inps

sumDigs 0 inp = inp
sumDigs n inp = head psums `seq` (sumDigs (n-1) . map toDig $ psums)
  where
    psums = scanr1 (+) inp
          

toDig int = int`rem`10
