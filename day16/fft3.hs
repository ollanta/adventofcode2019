import Text.Parsec
import qualified Data.Vector.Unboxed as V
import Helpers


main :: IO ()
main = do
  s <- getContents
  p <- either (error.show) return (readD s)
  putStrLn (showD $ solve p)


readD :: String -> Either ParseError [Int]
readD s = parse (many1 readC) "" s
  where
    readC = do
      d <- digit
      return (read [d])


showD = unlines . map (concatMap show)


solve :: [Int] -> [[Int]]
solve inps = [gen 0 msgoffset
             --, gen 1 msgoffset
             --, gen 100 msgoffset
             , gen 1 0
             , gen 100 0
             ]
  where
    msgoffset :: Int
    msgoffset = read . concatMap show . take 7 $ inps

    repcount = 10000
    totLength = repcount * (length inps)

    allinputs = V.fromListN totLength (cycle inps)

    gen g mo = V.toList . V.slice 0 8 . gen' g mo . V.drop mo $ allinputs

    gen' 0 mo vals = vals
    gen' n mo vals = gen' (n-1) mo (V.force vals')
      where
        psums = V.scanr1' (+) vals
        vals' = V.imap (\o _ -> calcDig mo psums (pattfor (mo+o))) psums

    calcDig mo psums patt = abs . (`rem`10) . sum . map (\(m,o) -> m * psums V.! (o-mo)) $ patt

    pattfor off = zip (cycle [1,-1,-1,1]) offsets
      where
        offsets = takeWhile (<totLength) [off,2*off+1..]
