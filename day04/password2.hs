import Text.Parsec


main :: IO ()
main = interact (show . solve . readD)


readD :: String -> (Int, Int)
readD s = p
  where
    Right p = parse readP "" s
    readP = do
      low  <- read <$> many digit
      char '-'
      high <- read <$> many digit
      return (low, high)


solve :: (Int, Int) -> Int
solve (low, high) = length . filter twoAdjacent . filter monotonic . map toIntList $ allPasswords
  where
    toIntList :: Int -> [Int]
    toIntList = map (read.(:[])) . show
    allPasswords  = [low..high]

    twoAdjacent :: [Int] -> Bool
    twoAdjacent [] = False
    twoAdjacent (i:is)
      | length dupes == 1 = True
      | otherwise         = twoAdjacent rest
      where
        (dupes, rest) = span (==i) is

    monotonic :: [Int] -> Bool
    monotonic [i] = True
    monotonic (i:j:js)
      | i <= j    = True && monotonic (j:js)
      | otherwise = False
