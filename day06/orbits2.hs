import Text.Parsec
import qualified Data.Set as  S
import Data.List

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
solve orbs = length combinedPath - length commonPath - 2
  where
    commonPath   = intersect santaPath youPath
    combinedPath = nub $ (santaPath ++ youPath)
    santaPath = find "COM" "SAN" []
    youPath = find "COM" "YOU" []
    find :: String -> String -> [String] -> [String]
    find oid goal os
      | oid == goal = goal : os
      | next == []  = []
      | otherwise   = concatMap (\(_,o) -> find o goal (oid:os)) next
        where next = filter ((==oid) . fst) orbs
