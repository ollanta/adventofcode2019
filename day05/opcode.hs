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
solve list = run 0 program
  where
    l = toInteger (length list - 1)
    program = A.listArray (0 :: Integer, l) list


run :: Integer -> A.Array Integer Integer -> [Integer]
run i p
  | op == 99 = []
  | op == 1  = run (i+4) padd
  | op == 2  = run (i+4) pmul
  | op == 3  = run (i+2) (p A.// [(r1, 1)])
  | op == 4  = (p A.! r1) : run (i+2) p
  where
    opcode = p A.! i
    op   = p A.! i `mod` 100
    mode = p A.! i `div` 100
    (m2,m1) = mode `divMod` 10

    r1 = p A.! (i+1)
    r2 = p A.! (i+2)
    r3 = p A.! (i+3)

    v1 = p A.! r1
    v2 = p A.! r2

    op1 = if m1 == 1 then r1 else v1
    op2 = if m2 == 1 then r2 else v2

    padd = p A.// [(r3, op1+op2)]
    pmul = p A.// [(r3, op1*op2)]
