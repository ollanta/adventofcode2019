import Text.Parsec
import qualified Data.Map as M

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


solve orbs = run 1 "COM"
  where
    run n oid = case filter ((==oid) . fst) orbs of
      [] -> 0
      os -> n * length os + sum rest
        where rest = map (run (n+1) . snd) os
