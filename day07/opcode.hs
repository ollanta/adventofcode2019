import qualified Data.Array as A
import Data.List

main :: IO ()
main = do
  interact (showD . solve . readD)
  putStrLn ""


readD :: String -> [Integer]
readD "" = []
readD s  = read s' : readD s'''
  where
    (s', s'') = span (/=',') s
    s''' = dropWhile (==',') s''


showD = show


toProgram ::[Integer] -> A.Array Integer Integer
toProgram list = program
  where
    l = toInteger (length list - 1)
    program = A.listArray (0 :: Integer, l) list


solve :: [Integer] -> (Integer, [Integer])
solve list = maximum $ map (\s -> (runAmps program s, s)) allSettings
  where
    program = toProgram list
    allSettings = permutations [0..4]


runAmps :: A.Array Integer Integer -> [Integer] -> Integer
runAmps program settings = last $ last outputs
  where
    inputs  = zipWith (\s output -> [s,last output]) settings ([0]:outputs)
    outputs = map (\input -> run 0 input program) inputs
{-
    a = run 0 [s1,0] program
    b = run 0 [s2,last a] program
    c = run 0 [s3,last b] program
    d = run 0 [s4,last c] program
    e = run 0 [s5,last d] program
-}

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
