import Text.Parsec
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Helpers
import Data.Char
import Data.Maybe
import Data.List


main :: IO ()
main = do
  s <- getContents
  putStrLn (showD . solve . readD $ s)


data Obj = Wall | Key Char | Door Char | Start | Open
  deriving (Eq)

cObj Wall = '#'
cObj (Key c) = c
cObj (Door c) = toUpper c
cObj Start = '@'
cObj Open = '.'

type Coord = (Integer, Integer)

readD :: String -> M.HashMap Coord Obj
readD s = M.fromList [((x,y), obj) | (y,objline) <- zip [0..] objLines, (x,obj) <- zip [0..] objline]
  where
    objLines = map (map toObj) (lines s)
    toObj '#' = Wall
    toObj '@' = Start
    toObj '.' = Open
    toObj c
      | isUpper c = Door c
      | isLower c = Key c
    
showD = show

isKey (Key _) = True
isKey _ = False


--solve :: M.HashMap Coord Obj -> Maybe Integer
solve omap = bfs 0 initS (S.toList initS) S.empty
  where
    startCoord :: Coord
    startCoord = head . M.keys . M.filter (==Start) $ omap
    nkeys = M.size . M.filter (isKey) $ omap

    initS = S.singleton (startCoord, S.empty, S.empty)

    bfs i seen [] next
      | S.null next = Nothing
      | otherwise   = bfs (i+1) seen (S.toList next) S.empty
    bfs i seen ((c, keys, doors):cks) next
      | not (null newNext) && longest == nkeys = Just (i+1, c, keys, doors)
      | otherwise = bfs i seen' cks next'
      where
        newcoords = neighbors c
        newNext = filter (\t -> not $ S.member t seen) . mapMaybe toTuple $ newcoords

        longest = maximum . map (\(_,k,_) -> S.size k) $ newNext

        next' = foldl' (\s v -> S.insert v s) next newNext
        seen' = foldl' (\s v -> S.insert v s) seen newNext

        toTuple c = case omap M.! c of
          Wall -> Nothing
          Key ch -> Just (c, S.insert ch keys, doors)
          Door ch -> case S.member (toLower ch) keys of
            True -> Just (c, keys, S.insert ch doors)
            False -> Nothing
          _    -> Just (c, keys, doors)


neighbors (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
