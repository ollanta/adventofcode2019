import Text.Parsec
import Data.Tree


main :: IO ()
main = interact (show . solve . readD)


readD :: String -> [(String, String)]
readD s = orbits
  where
    Right orbits = parse readOS "" s
    readOS = readO `endBy` newline <* eof
    readO = do
      p1 <- many alphaNum
      char ')'
      p2 <- many alphaNum
      return (p1, p2)


solve :: [(String,String)] -> Int
solve orbs = nOrbits
  where
    orbitTree = unfoldTree (\oid -> (oid, orbiting oid)) "COM"
    orbiting oid = map snd . filter ((==oid) . fst) $ orbs
    nOrbits = sum $ zipWith (*) [0..] (map length $ levels orbitTree)
