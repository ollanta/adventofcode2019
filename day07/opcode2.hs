import qualified Data.Array as A
import Data.List

main :: IO ()
main = interact (showD . solve . readD)

readD :: String -> [Integer]
readD "" = []
readD s  = read s' : readD s'''
  where
    (s', s'') = span (/=',') s
    s''' = dropWhile (==',') s''


--showD :: [Integer] -> String
showD = unlines . map show . sort -- . head

toProgram ::[Integer] -> A.Array Integer Integer
toProgram list = program
  where
    l = toInteger (length list - 1)
    program = A.listArray (0 :: Integer, l) list


solve :: [Integer] -> [([Integer], [Integer])]
solve list = map (\s -> (runAmps program s, s)) allSettings
  where
    program = toProgram list
    allSettings :: [[Integer]]
    allSettings = [[s1,s2,s3,s4,s5] | s1 <- [5..9], s2 <- [5..9], s3 <- [5..9], s4 <- [5..9], s5 <- [5..9], s1 /= s2, s1 /= s3, s1 /= s4, s1 /= s5, s2/=s3, s2/=s4, s2/=s5, s3/=s4, s3/=s5, s4/=s5]

runAmps :: A.Array Integer Integer -> [Integer] -> [Integer]
runAmps program [s1,s2,s3,s4,s5] = e
  where
    a = run 0 (s1:0:e) program
    b = run 0 (s2:a) program
    c = run 0 (s3:b) program
    d = run 0 (s4:c) program
    e = run 0 (s5:d) program


run :: Integer -> [Integer] -> A.Array Integer Integer -> [Integer]
run i rs p
  | op == 99 = []
  | op == 1  = run (i+4) rs padd
  | op == 2  = run (i+4) rs pmul
  | op == 3  = run (i+2) (drop 1 rs) (p A.// [(r1, head rs)])
  | op == 4  = (p A.! r1) : run (i+2) rs p
  | op == 5  = run (jumpif True)  rs p
  | op == 6  = run (jumpif False) rs p
  | op == 7  = run (i+4) rs pless
  | op == 8  = run (i+4) rs pequals
  where
    opcode = p A.! i
    op   = p A.! i `mod` 100
    mode = p A.! i `div` 100
    (m2,m1) = mode `divMod` 10

    r1 = p A.! (i+1)
    r2 = p A.! (i+2)

    v1 = p A.! r1
    v2 = p A.! r2

    op1 = if m1 == 1 then r1 else v1
    op2 = if m2 == 1 then r2 else v2

    r3 = p A.! (i+3)

    padd = p A.// [(r3, op1+op2)]
    pmul = p A.// [(r3, op1*op2)]
    pless   = p A.// [(r3, if op1<op2 then 1 else 0)]
    pequals = p A.// [(r3, if op1==op2 then 1 else 0)]

    jumpif b
      | b == (op1 > 0) = op2
      | otherwise      = i+3
