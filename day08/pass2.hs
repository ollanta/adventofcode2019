import Text.Parsec
import Data.Ord
import Data.List
import Data.Function


main :: IO ()
main = do
  interact (showD . solve . readD)
  putStrLn ""


showD :: [Integer] -> String
showD ls = unlines . map (concatMap showC) $ helper ls
  where
    showC 1 = "X"
    showC 0 = " "
    helper [] = []
    helper l  = take 25 l : helper (drop 25 l)


readD :: String -> [Integer]
readD s = digits
  where
    Right digits = parse (many oneDigit) "" s
    oneDigit = digitToInt <$> digit
    digitToInt c = read (c:[])


toLayers :: [a] -> [[a]]
toLayers [] = []
toLayers l = take (6*25) l : toLayers (drop (6*25) l)


countl :: Int -> [Integer] -> Int
countl c l = length $ filter ((==c) . fromInteger) l


overlap :: [Integer] -> [Integer] -> [Integer]
overlap = zipWith cmerge
  where
    cmerge 2 b = b
    cmerge f _ = f


solve :: [Integer] -> [Integer]
solve l = foldr1 overlap layers
  where
    layers = toLayers l
