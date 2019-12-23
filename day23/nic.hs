import qualified Data.HashMap.Strict as M
import Helpers
import Intcode
import Data.Char
import Data.Maybe

main :: IO ()
main = do
  s <- getContents
  putStrLn (showD . solve . readProgram $ s)

showD = show


solve program = msg
  where
    nics = [0..49]
    
    initComps = [runOne (initComputer program) nic | nic <- nics]

    compMap = M.fromList $ zip nics initComps

    compStates = iterate (uncurry updateStates) (compMap, M.empty)

    msg = M.lookup 255 . snd . head . dropWhile (not . M.member 255 . snd) $ compStates


updateStates pmap pqueue = update' pmap pqueue (M.keys pmap)
  where
    update' pmap pqueue [] = (pmap, pqueue)
    update' pmap pqueue (nic:rest)
      | isNothing comp = update' (M.delete nic pmap) pqueue rest
      | otherwise      = update' (M.insert nic (restout ++ newout, c') pmap) pqueue'' rest
      where
        (oldoutput, comp) = pmap M.! nic
        Just c = comp

        nextinp = take 2 $ M.lookupDefault [] nic pqueue
        (newout, c') = runW c nextinp

        pqueue' = M.adjust (drop 2) nic pqueue

        output = oldoutput ++ newout
        fullmsgs = length output `div` 3
        (msgout, restout) = splitAt ((length output `div` 3) * 3) output
        messages = toTriples msgout

        pqueue'' = insertOut messages pqueue'

        runW c []    = runOne c (-1)
        runW c [x,y] = (o1 ++ o2, c2)
          where
            (o1, Just c1) = runOne c x
            (o2, c2) = runOne c1 y


insertOut outp pq = foldl (\apq (to,x,y) -> M.alter (addToQueue (x,y)) to apq) pq outp
  where
    addToQueue (x,y) Nothing = Just [x,y]
    addToQueue (x,y) (Just l) = Just $ l ++ [x,y]


toTriples (a:b:c:rest) = (a,b,c):toTriples rest
toTriples [] = []
