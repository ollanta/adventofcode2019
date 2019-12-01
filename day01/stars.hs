
main :: IO ()
main = interact solve
  where solve = show . sum . map (calcFuel . read) . lines

calcFuel :: Integer -> Integer
calcFuel m = m `div` 3 - 2
