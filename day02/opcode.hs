import qualified Data.Array as A

main :: IO ()
main = interact (showD . solve . readD)

readD :: String -> [Integer]
readD "" = []
readD s  = read s' : readD s'''
  where
    (s', s'') = span (/=',') s
    s''' = dropWhile (==',') s''


showD :: [Integer] -> String
showD = show -- . head

solve :: [Integer] -> [Integer]
solve list = A.elems (run 0 program')
  where
    l = toInteger (length list - 1)
    program = A.listArray (0 :: Integer, l) list
    program' = program A.// [(1,12), (2,2)]


run :: Integer -> A.Array Integer Integer -> A.Array Integer Integer
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
