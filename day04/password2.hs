import Text.Parsec


main :: IO ()
main = interact (show . solve . readD)


readD :: String -> (Int, Int)
readD s = (low, high)
  where
    Right [low, high] = parse readP "" s
    readP = (read <$> many digit) `sepBy` char '-'


solve :: (Int, Int) -> Int
solve (low, high) = length . filter twoAdjacent . filter monotonic . map show $ [low..high]
  where
    twoAdjacent is@(i:_)
      | length dupes == 2 = True
      | otherwise         = twoAdjacent rest
      where
        (dupes, rest) = span (==i) is
    twoAdjacent [] = False

    monotonic (i:j:js)
      | i <= j    = True && monotonic (j:js)
      | otherwise = False
    monotonic [i] = True
