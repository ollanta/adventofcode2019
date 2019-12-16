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
solve inps = [take 8 $ loop 1 inps, take 8 $ loop 100 inps]
  where
    origPattern i = [replicate i 0, replicate i 1, replicate i 0, replicate i (-1)]
    patt i = drop 1 . cycle . concat $ origPattern i
    patterns = map patt [1..]

    loop :: Int -> [Integer] -> [Integer]
    loop 0 inp = inp
    loop i inp = loop (i-1) inp'
      where
        n = length inp
        inp' = map (toDig . sum) . take n $ zipWith (zipWith (*)) patterns (repeat inp)

    toDig int = abs(int`rem`10)

