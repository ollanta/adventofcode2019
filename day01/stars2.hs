
main :: IO ()
main = interact solve
  where
    solve = show . sum . map (calcAllFuel . read) . lines

calcFuel :: Integer -> Integer
calcFuel m = m `div` 3 - 2

calcAllFuel :: Integer -> Integer
calcAllFuel m = sum . takeWhile (>0) $ fuelSeries
  where
    fuelSeries = drop 1 $ iterate calcFuel m
