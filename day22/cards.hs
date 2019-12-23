import Text.Parsec
import qualified Data.Vector.Unboxed as V
import Helpers
import Data.List


main :: IO ()
main = do
  s <- getContents
  p <- either (error.show) return (readD s)
  putStrLn (showD $ solve p)


data Shuffle = Cut Int | DealWith Int | DealInto
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


solve shuffles = V.findIndex (==2019) shuffledStack
  where
    initStack :: V.Vector Int
    initStack = V.fromList [0..10006]

    shuffledStack = ishuffle shuffles initStack
    ishuffle (s:ss) stack = ishuffle ss stack'
      where
        stack' = shuffle stack s
    ishuffle [] s = s


shuffle stack (Cut n) = cut n stack
shuffle stack (DealWith n) = dealWith n stack
shuffle stack DealInto = dealInto stack


dealInto stack = V.reverse stack


cut n stack
  | n >= 0 = V.concat [rest, first]
  | n <  0 = cut (l+n) stack
  where
    l = V.length stack
    (first, rest) = V.splitAt n stack


dealWith incn stack = stack V.// updates
  where
    l = V.length stack
    indices = take l . map (`mod`l) $ [0,incn..]
    updates = zipWith (\oi i -> (i, stack V.! oi)) [0..] indices
