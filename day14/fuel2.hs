import Text.Parsec
import Data.List as L
import qualified Data.HashMap.Strict as M
import Helpers


main :: IO ()
main = do
  s <- getContents
  p <- either (error.show) return (readD s)
  putStrLn (showD $ solve p)


type Dependency = ([(Integer, String)], (Integer, String))

type Req = (Integer, String)

readD :: String -> Either ParseError [Dependency]
readD s = parse (readReq `endBy` newline) "" s
  where
    readReq = do
      prereqs <- (read1req `sepBy` string ", ")
      spaces
      string "=>"
      spaces
      result <- read1req
      return (prereqs, result)
    read1req = do
      n <- many digit
      space
      s <- many alphaNum
      return (read n :: Integer, s)


showD = unlines . map show


solve :: [Dependency] -> [(Integer, Integer)]
solve deps = search minFuel maxFuel
  where
    maxCost = 1000000000000
    minFuel = div maxCost (calcCost 1)
    maxFuel = head . dropWhile ((<maxCost) . calcCost) $ [m*minFuel | m <- [2,4..]]

    search min max
      | min == max-1 = [(min,max)]
      | calcCost mid > maxCost = (min,max):search min mid
      | otherwise = (min,max):search mid max
      where
        mid = (min + max) `div` 2

    calcCost n = snd $ costOf depmap n "FUEL" M.empty
    depmap = M.fromList $ map (\(req,(n, t)) -> (t,(n,req))) deps


costOf :: M.HashMap String (Integer, [(Integer,String)]) -> Integer -> String -> M.HashMap String Integer -> (M.HashMap String Integer, Integer)
costOf depmap n "ORE" m = (m, fromInteger n)
costOf depmap n s m     = (m''', cost)
  where
    (dn, reqs) = depmap M.! s

    multiplier = ceilDiv n dn
    rest       = multiplier*dn - n

    mreqs = map (\(n,t) -> (multiplier*n, t)) reqs
    (m', reducedreqs) = reducereq [] m mreqs

    (m'', cost) = includereq depmap 0 m' reducedreqs

    m''' = M.insert s rest m''


includereq :: M.HashMap String (Integer, [(Integer,String)]) -> Integer -> M.HashMap String Integer -> [Req] -> (M.HashMap String Integer, Integer)
includereq depmap acc m [] = (m, acc)
includereq depmap acc m (req@(n,t):rs) = includereq depmap (acc+cost) m' rs
  where
    (m', cost) = costOf depmap n t m


reducereq :: [Req] -> M.HashMap String Integer -> [Req] -> (M.HashMap String Integer, [Req])
reducereq acc m [] = (m,acc)
reducereq acc m (req@(n,t):rs)
  | have == 0 = reducereq (req:acc) m rs
  | have >= n = reducereq acc (M.insert t (have-n) m) rs
  | otherwise = reducereq ((n-have, t):acc) (M.insert t 0 m) rs
  where
    have = M.lookupDefault 0 t m
