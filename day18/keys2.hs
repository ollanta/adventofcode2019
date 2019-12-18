import Text.Parsec
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Helpers
import Data.Char
import Data.Maybe
import Data.List
import qualified Data.Heap as H


main :: IO ()
main = do
  s <- getContents
  putStrLn (showD . solve . readD $ s)


data Obj = Wall | Key Char | Door Char | Start Int | Open
  deriving (Eq, Show)

type Coord = (Integer, Integer)
type Coord4T = (Coord, Coord, Coord, Coord)
type CharSet = S.HashSet Char
type SetKey = (Coord4T, CharSet)

readD :: String -> M.HashMap Coord Obj
readD s = M.fromList [((x,y), obj) | (y,objline) <- zip [0..] objLines, (x,obj) <- zip [0..] objline]
  where
    objLines = map (map toObj) (lines s)
    toObj '#' = Wall
    toObj '@' = Start 0
    toObj '.' = Open
    toObj c
      | isUpper c = Door c
      | isLower c = Key c
    

showD = show


isDoor (Door _) = True
isDoor _ = False

isKey (Key _) = True
isKey _ = False


neighbors (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]


solve omap = result
  where
    starts = M.toList . M.filter (==Start 0) $ omap
    starts' = zipWith (\i (c,s) -> (c ,Start i)) [1..] $ starts
    omap' = M.union (M.fromList starts') omap
    keys = M.toList . M.filter isKey $ omap
    doors = M.toList . M.filter isDoor $ omap

    -- generate graph of distances between interesting points
    allItems = starts' ++ keys ++ doors
    distances :: M.HashMap Coord [(Obj, Coord, Integer)]
    distances = M.fromList [(ac, [(b,bc, fromJust dist) |
                                  (bc,b) <- allItems,
                                  a /= b,
                                  let dist = bfsFromTo ac bc,
                                  dist /= Nothing
                                  ])
                           | (ac,a) <- allItems]

    bfsFromTo from to = bfs to 0 (S.singleton from) [from] S.empty

    bfs :: Coord -> Integer -> S.HashSet Coord -> [Coord] -> S.HashSet Coord -> Maybe Integer
    bfs to i seen [] next
      | S.null next = Nothing
      | otherwise   = bfs to (i+1) seen (S.toList next) S.empty
    bfs to i seen (c:cs) next
      | c == to              = Just i
      | any (==to) newcoords = Just (i+1)
      | otherwise = bfs to i seen' cs next'
      where
        newcoords = neighbors c
        newNext = filter (not . (`S.member` seen)) . mapMaybe toTuple $ newcoords

        next' = foldr S.insert next newNext
        seen' = foldr S.insert seen newNext

        toTuple c = case omap M.! c of
          Wall -> Nothing
          Door _ -> Nothing
          _    -> Just c


    [s1,s2,s3,s4] = map fst starts'
    startst = (s1,s2,s3,s4)
    result = search S.empty (H.singleton (0, (startst, S.empty)))

    nkeys = length keys

    --search :: [(Integer, (Coord4T, S.HashSet Char))] -> Maybe Integer
    --search seen ((d, ((s1, s2, s3, s4), keys)):next)
    search :: S.HashSet SetKey -> H.MinPrioHeap Integer (Coord4T, CharSet) -> Maybe Integer
    search seen heap
      | H.isEmpty heap       = Nothing
      | length keys == nkeys = Just d
      | otherwise            = search seen' heap''
      where
        Just (item, heap') = H.view heap
        (d, ((s1, s2, s3, s4), keys)) = item

        newstates1 = [(d+d', ((nc, s2, s3, s4), keys')) |
                      let nss = M.lookupDefault [] s1 distances,
                      (no, nc, d') <- nss,
                      isOk no keys,
                      let keys' = updateKeys no keys]
        newstates2 = [(d+d', ((s1, nc, s3, s4), keys')) |
                      let nss = M.lookupDefault [] s2 distances,
                      (no, nc, d') <- nss,
                      isOk no keys,
                      let keys' = updateKeys no keys]
        newstates3 = [(d+d', ((s1, s2, nc, s4), keys')) |
                      let nss = M.lookupDefault [] s3 distances,
                      (no, nc, d') <- nss,
                      isOk no keys,
                      let keys' = updateKeys no keys]
        newstates4 = [(d+d', ((s1, s2, s3, nc), keys')) |
                      let nss = M.lookupDefault [] s4 distances,
                      (no, nc, d') <- nss,
                      isOk no keys,
                      let keys' = updateKeys no keys]

        newostates = filter (\(_,st) -> not $ S.member st seen) $ newstates1 ++ newstates2 ++ newstates3 ++ newstates4
        seen' = foldl' (\s (_,st) -> S.insert st s) seen newostates

        heap'' = H.union heap' (H.fromList newostates)

    isOk (Door c) keys = S.member (toLower c) keys
    isOk (Key c) keys  = not $ S.member c keys
    isOk (Start _) _   = False

    updateKeys (Key c) keys = S.insert c keys
    updateKeys _ keys = keys
