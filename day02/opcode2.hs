import qualified Data.Array as A


main :: IO ()
main = interact (showD . solve . readD)


readD :: String -> [Integer]
readD "" = []
readD s  = read s' : readD s'''
  where
    (s', s'') = span (/=',') s
    s''' = dropWhile (==',') s''


type Program = A.Array Integer Integer


showD :: [(Integer, Integer)] -> String
showD = unlines . map showD'
  where
    showD' (noun, verb) = "noun:" ++ show noun ++ " verb:" ++ show verb ++ " sum:" ++ show (noun*100+verb)


solve :: [Integer] -> [(Integer, Integer)]
solve list = runs
  where
    l = toInteger (length list) - 1
    program = A.listArray (0, l) list
    goal = 19690720
    runs = [(noun, verb) | noun <- [0..99], verb <- [0..99], let p' = initRun noun verb program, p' A.! 0 == goal]


initRun :: Integer -> Integer -> Program -> Program
initRun noun verb program = run 0 program'
  where
    program' = program A.// [(1,noun), (2,verb)]


run :: Integer -> Program -> Program
run i p
  | op == 99 = p
  | op == 1  = run (i+4) padd
  | op == 2  = run (i+4) pmul
  where
    op = p A.! i
    r1 = p A.! (i+1)
    r2 = p A.! (i+2)
    v1 = p A.! r1
    v2 = p A.! r2
    r3 = p A.! (i+3)
    padd = p A.// [(r3, v1+v2)]
    pmul = p A.// [(r3, v1*v2)]
