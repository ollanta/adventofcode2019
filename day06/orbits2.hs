import Text.Parsec
import Data.Tree
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


data Distance = Not | Count Int | Dist Int
  deriving (Eq, Show)

instance Semigroup Distance where
  Dist d <> _ = Dist d
  _ <> Dist d = Dist d

  Count a <> Count b = Dist (a+b)
  Count a <> _ = Count a
  _ <> Count b = Count b

  Not <> Not = Not


solve orbs = distance
  where
    orbitTree = unfoldTree (\oid -> (oid, orbiting oid)) "COM"
    orbiting oid = map snd . filter ((==oid) . fst) $ orbs

    Dist distance = foldTree countDist $ fmap isSanOrYou orbitTree
    isSanOrYou "YOU" = Count 0
    isSanOrYou "SAN" = Count 0
    isSanOrYou _     = Not

    countDist d ds = d <> increment childsum
      where
        childsum = foldr (<>) Not ds
        increment (Dist d) = Dist d
        increment (Count d) = Count (d+1)
        increment Not = Not
