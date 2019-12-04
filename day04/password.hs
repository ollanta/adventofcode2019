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
    twoAdjacent [i] = False
    twoAdjacent (i:j:_)
      | i == j    = True
    twoAdjacent (_:rest) = twoAdjacent rest

    monotonic :: [Int] -> Bool
    monotonic [i] = True
    monotonic (i:j:js)
      | i <= j    = True && monotonic (j:js)
      | otherwise = False
