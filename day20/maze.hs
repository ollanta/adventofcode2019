import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Helpers
import Data.Maybe
import Data.List


main :: IO ()
main = do
  s <- getContents
  putStrLn (showD . solve . readD $ s)


data Obj = Wall | Port String | Open | EmptySpace
  deriving (Eq, Show)

type Coord = (Integer, Integer)

readD :: String -> M.HashMap Coord Obj
readD s = M.fromList [((x,y), obj) | (y,objline) <- zip [0..] objLines, (x,obj) <- zip [0..] objline]
  where
    objLines = map (map toObj) (lines s)
    toObj '#' = Wall
    toObj '.' = Open
    toObj ' ' = EmptySpace
    toObj c   = Port [c]

showD = unlines

drawm = drawMap vObj
  where
    vObj Wall = '#'
    vObj Open = '.'
    vObj EmptySpace =  ' '
    vObj (Port s) = head s


isPort (Port _) = True
isPort _ = False

fromPort (Port s) = s

solve premap = [drawm omap
               , show $ bfs 0 (S.singleton startCoord) [startCoord] S.empty
               ]
  where
    omap = mergePorts premap

    portCoords = getPortcoords omap

    startCoord = head $ portCoords M.! "AA"
    endCoord = head $ portCoords M.! "ZZ"

    bfs i seen [] next
      | S.null next = Nothing
      | otherwise   = bfs (i+1) seen (S.toList next) S.empty
    bfs i seen (c:cs) next
      | found     = Just (i+1)
      | otherwise = bfs i seen' cs next'
      where
        newcoords = neighbors c
        newNext = filter (\t -> not $ S.member t seen) . mapMaybe filterNext $ newcoords

        found = any (==endCoord) newNext

        next' = foldl' (\s v -> S.insert v s) next newNext
        seen' = foldl' (\s v -> S.insert v s) seen newNext

        filterNext :: Coord -> Maybe Coord
        filterNext nc = case omap M.! nc of
          Wall -> Nothing
          EmptySpace -> Nothing
          Port s -> case filter (/=c) (portCoords M.! s) of
            [] -> Nothing
            [nc'] -> Just nc'
          _    -> Just nc


getPortcoords omap = portCoords
  where
    portalC :: String -> [Coord]
    portalC s = map openSpace portc
      where
        openSpace pc = head . map fst . filter (\(c,o) -> o==Open) . map (\c -> (c, omap M.! c)) $ (neighbors pc)
        portc = M.keys . M.filter (==Port s) $ omap

    portals = M.filter isPort omap

    portCoords = M.fromList . map (\s -> (s, portalC s)) . map fromPort $ M.elems portals


mergePorts omap = M.unions [remports, ports', omap]
  where
    ports = M.filter isPort omap
    lookupd c = M.lookupDefault EmptySpace c omap

    toMainPort :: Coord -> Obj -> Maybe Obj
    toMainPort (x,y) p
      | any (==Open) hori = Just . Port $ fixName hori
      | any (==Open) vert = Just . Port $ fixName vert
      | otherwise         = Nothing
      where
        hori = map lookupd [(x',y) | x' <- [x-1..x+1]]
        vert = map lookupd [(x,y') | y' <- [y-1..y+1]]
        fixName = concat . map fromPort . filter isPort

    ports' = M.mapMaybeWithKey toMainPort ports

    remports = M.map (\_ -> EmptySpace) $ M.difference ports ports'


neighbors (x,y) = [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]
