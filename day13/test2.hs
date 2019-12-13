import qualified Data.HashMap.Strict as M
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


showD = unlines


toProgram ::[Integer] -> M.HashMap Integer Integer
toProgram list = program
  where
    program = M.fromList $ zip [0..] list


type Coord = (Integer, Integer)


draw :: M.HashMap Coord Integer -> String
draw pmap = unlines ["Score: " ++ show score,
                     unlines showMap]
  where
    score = (M.lookupDefault 0 (-1,0) pmap)
    pmap' = M.delete (-1,0) pmap
    sx = minimum . map fst . M.keys $ pmap'
    ex = maximum . map fst . M.keys $ pmap'
    sy = minimum . map snd . M.keys $ pmap'
    ey = maximum . map snd . M.keys $ pmap'
    draw 0 = ' '
    draw 1 = 'W'
    draw 2 = 'B'
    draw 3 = '_'
    draw 4 = 'o'

    showMap = reverse [[draw (M.lookupDefault 0 (x,y) pmap') | x <- [sx..ex]] | y <- [sy..ey]]


split (x1:x2:x3:xs) = let (xr1,xr2,xr3) = split xs in (x1:xr1, x2:xr2, x3:xr3)
split [] = ([],[],[])


lastn i l@(_:rest)
  | shorter i l = l
  | otherwise   = lastn i rest
  where
    shorter _ [] = True
    shorter 0 _  = False
    shorter i (l:ls) = shorter (i-1) ls


solve :: [Integer] -> [String]
solve list = lastn 5 $ map draw interesting
  where
    program = toProgram list

    hasPaddle = not . M.null . M.filter (==3)
    hasBall = not . M.null . M.filter (==4)
    getPaddle = head . M.keys . M.filter (==3)
    getBall = head . M.keys . M.filter (==4)

    interesting = filter (\m -> hasPaddle m && hasBall m) maps
    ps = map getPaddle interesting
    bs = map getBall interesting

    inputs = 0:botinp ps bs

    outputs = run (Computer 0 0 inputs program)

    (xs, ys, ts) = split outputs
    instrs = zip (zip xs ys) ts

    maps = scanl (\m (c,t) -> M.insert c t m) M.empty instrs


botinp (p1:restp@(p2:ps)) (b1:restb@(b2:bs))
  | b1 == b2  = cont
  | px <  b2x = 1 : cont
  | px >  b2x = -1 : cont
  | otherwise = 0 : cont
  where
    cont = botinp restp restb
    (px,_) = p1
    (b2x,_) = b2
botinp _ _ = []


data Computer = Computer {
  pointer :: Integer,
  relPointer :: Integer,
  inputs :: [Integer],
  program :: M.HashMap Integer Integer
}


data Param = Pos !Integer | Val !Integer


run :: Computer -> [Integer]
run c@Computer{pointer=i, relPointer=reli, inputs=rs, program=prog}
  | op == 99 = []
  | op == 1  = run c{pointer=i+4, program=padd}
  | op == 2  = run c{pointer=i+4, program=pmul}
  | op == 3  = run c{pointer=i+2, inputs=readRest, program=pinsert (arg 1) readNext}
  | op == 4  = rarg 1 : run c{pointer=i+2}
  | op == 5  = run c{pointer=jumpif True}
  | op == 6  = run c{pointer=jumpif False}
  | op == 7  = run c{pointer=i+4, program=pless}
  | op == 8  = run c{pointer=i+4, program=pequals}
  | op == 9  = run c{pointer=i+2, relPointer=reli + rarg 1}
  where
    plookup (Pos k) = M.lookupDefault 0 k prog
    plookup (Val v) = v
    pinsert (Pos k) param = M.insert k param prog

    opcode:parameters = map (plookup . Pos) [i..]

    (modecode, op) = opcode `divMod` 100

    modes = parseModecode modecode

    args = [
      case m of
        0 -> Pos param
        1 -> Val param
        2 -> Pos (reli + param)
      | (m, param) <- zip modes parameters
      ]
    arg i = args !! (i-1)
    rarg i = plookup (arg i)

    readNext:readRest = rs

    padd = pinsert (arg 3) (rarg 1 + rarg 2)
    pmul = pinsert (arg 3) (rarg 1 * rarg 2)
    pless   = pinsert (arg 3) (if rarg 1 < rarg 2 then 1 else 0)
    pequals = pinsert (arg 3) (if rarg 1 == rarg 2 then 1 else 0)

    jumpif b
      | b == (rarg 1 > 0) = rarg 2
      | otherwise         = i+3


parseModecode :: Integer -> [Integer]
parseModecode modecode = unfoldr (\m -> Just . flipP $ m `divMod` 10) modecode
  where
    flipP (a,b) = (b,a)
