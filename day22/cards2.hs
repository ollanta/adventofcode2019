import Text.Parsec
import Helpers
import Data.List
import qualified Data.Matrix as M


main :: IO ()
main = do
  s <- getContents
  p <- either (error.show) return (readD s)
  putStrLn (showD $ solve p)


data Shuffle = Cut Integer | DealWith Integer | DealInto
  deriving (Eq, Show)


readD :: String -> Either ParseError [Shuffle]
readD s = parse (readShuffle `endBy` newline) "" s
  where
    readShuffle = choice [readCut, readDealWith, readDealInto]
    number = fmap read $ many (oneOf "-1234567890")
    readCut = Cut <$> (string "cut " *> number)
    readDealWith = DealWith <$> (try (string "deal with increment ") *> number)
    readDealInto = const DealInto <$> (try (string "deal into new stack"))


showD = show


solve shuffles = (allshuffles,
                  leftinv,
                  modMatrix (leftinv * allshuffles) stackl,
                  modMatrix (leftinv * M.fromList 2 1 [2020,1]) stackl
                 )
  where
    stackl :: Integer
    stackl = 119315717514047

    nshuffles :: Integer
    nshuffles = 101741582076661

    fullshuffle = merge stackl shuffles

    powshuffles = iterate (\t -> modMatrix (t*t) stackl) fullshuffle

    powshufflePairs = takeWhile ((<nshuffles) . fst) $ zip [2^n | n <- [0..]] powshuffles

    allshuffles = loop nshuffles (M.identity 2) (reverse powshufflePairs)
      where
        loop 0 m _ = m
        loop n m ((i,sh):rest)
          | i <= n     = loop (n-i) m' rest
          | otherwise = loop n m rest
          where
            m' = modMatrix (m*sh) stackl

    leftinv = modMatrix (minv stackl allshuffles) stackl


minv :: Integer -> M.Matrix Integer -> M.Matrix Integer
minv l m = M.fromList 2 2 [x, y, 0, 1]
  where
    a = fromInteger $ M.getElem 1 1 m
    b = fromInteger $ M.getElem 1 2 m
    x = invMod a l
    y = -b * x


merge l shufs = modMatrix mergedMat l
  where
    mergedMat = foldr1 (*) . map (move l) $ reverse shufs


modMatrix mat m = M.fromLists . map (map (`mod` m)) $ M.toLists mat


move l (Cut c)      = M.fromList 2 2 [ 1, -c, 0, 1]
move l (DealWith w) = M.fromList 2 2 [ w,  0, 0, 1]
move l DealInto     = M.fromList 2 2 [-1,l-1, 0, 1]


-- this took an age to figure out
invMod 0 _ = undefined
invMod 1 _ = 1
invMod w l
  | w < 0     = invMod (w+l) l
  | otherwise = k + d * p
  where
    (d,r) = l `divMod` w
    p = invMod (w-r) w
    k = 1 + p * r `div` w


apply l shufs = map snd . sort $ zip ixmove [0..]
  where
    mshufs = merge l shufs

    ixmove = map (\i -> (`mod`l) . M.getElem 1 1 . (mshufs *) $ M.fromList 2 1 [i,1]) [0..l-1]
