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

    statesWithRestart = dropWhile (not . M.member 256 . snd) $ compStates
    statesWithRepeats = dropWhile (dropCondition . snd) $ statesWithRestart

    msg = lastN 4 . (M.! 256) . snd . head $ statesWithRepeats

    dropCondition pq
      | length last4 /= 4 = True
      | otherwise         = a /= b
      where
        natvals = pq M.! 256
        last4 = lastN 4 natvals
        [_,a,_,b] = last4


updateStates pmap pqueue = update' True pmap pqueue (M.keys pmap)
  where
    update' allIdle pmap pqueue []
      | allIdle   = (pmap, pqueue'')
      | otherwise = (pmap, pqueue)
      where
        [x,y]   = lastN 2 (pqueue M.! 255)
        pqueue' = M.insert 0 [x,y] pqueue
        -- use address 256 for tracking restarts
        pqueue'' = insertOut [(256,x,y)] pqueue'
    update' allIdle pmap pqueue (nic:rest)
      | isNothing comp = update' allIdle (M.delete nic pmap) pqueue rest
      | otherwise      = update' (allIdle && idle) (M.insert nic (restout ++ newout, c') pmap) pqueue'' rest
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

        idle = nextinp == [] && newout == []

        runW c []    = runOne c (-1)
        runW c [x,y] = runMulti c [x,y]


insertOut outp pq = foldl (\apq (to,x,y) -> M.alter (addToQueue (x,y)) to apq) pq outp
  where
    addToQueue (x,y) Nothing = Just [x,y]
    addToQueue (x,y) (Just l) = Just $ l ++ [x,y]


toTriples (a:b:c:rest) = (a,b,c):toTriples rest
toTriples [] = []
