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


update4t 1 v (_,t2,t3,t4) = (v ,t2,t3,t4)
update4t 2 v (t1,_,t3,t4) = (t1,v ,t3,t4)
update4t 3 v (t1,t2,_,t4) = (t1,t2,v ,t4)
update4t 4 v (t1,t2,t3,_) = (t1,t2,t3,v )

listTo4t [a,b,c,d] = (a,b,c,d)

t4toList (a,b,c,d) = [a,b,c,d]


solve omap = result
  where
    starts = M.toList . M.filter (==Start 0) $ omap
    starts' = zipWith (\i (c,s) -> (c ,Start i)) [1..] $ starts
    keys = M.toList . M.filter isKey $ omap
    doors = M.toList . M.filter isDoor $ omap

    -- generate graph of distances between interesting points
    allItems = starts' ++ keys ++ doors
    distances = getDistanceMap omap allItems

    -- search for a final state from starts
    startst = listTo4t $ map fst starts'
    result = search S.empty (H.singleton (0, (startst, S.empty)))

    nkeys = length keys

    search :: S.HashSet SetKey -> H.MinPrioHeap Integer (Coord4T, CharSet) -> Maybe Integer
    search seen heap
      | H.isEmpty heap       = Nothing
      | length keys == nkeys = Just d
      | otherwise            = search seen' heap''
      where
        Just ((d, state), heap') = H.view heap
        (coordst, keys) = state

        stateupdates = concatMap (\(i,c) -> zip (repeat i) (genstates c keys)) $ zip [1..] (t4toList coordst)
        newostates = [(d+d', (update4t i c' coordst, keys')) |
                      (i, (d', c', keys')) <- stateupdates]

        newostates' = filter (\(_,st) -> not $ S.member st seen) $ newostates
        seen' = foldl' (\s (_,st) -> S.insert st s) seen newostates'

        heap'' = H.union heap' (H.fromList newostates')

    genstates c keys = [(d, c', keys') |
                        (no, c', d) <- M.lookupDefault [] c distances,
                        isOk no keys,
                        let keys' = updateKeys no keys]

    isOk (Door c) keys = S.member (toLower c) keys
    isOk (Key c) keys  = not $ S.member c keys
    isOk (Start _) _   = False

    updateKeys (Key c) keys = S.insert c keys
    updateKeys _ keys = keys


getDistanceMap :: M.HashMap Coord Obj -> [(Coord, Obj)] -> M.HashMap Coord [(Obj, Coord, Integer)]
getDistanceMap omap items = M.fromList [(ac, [(b, bc, fromJust dist) |
                                              (bc,b) <- items,
                                              a /= b,
                                              let dist = bfsFromTo ac bc,
                                                  dist /= Nothing])
                                       | (ac,a) <- items]
  where
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

neighbors (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
