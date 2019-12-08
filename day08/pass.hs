import Text.Parsec
import Data.Ord
import Data.List
import Data.Function


main :: IO ()
main = do
  interact (show . solve . readD)
  putStrLn ""


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


solve :: [Integer] -> Int
solve l = (countl 1 layer) * (countl 2 layer)
  where
    layers = toLayers l
    layer  = minimumBy (comparing (countl 0)) layers
