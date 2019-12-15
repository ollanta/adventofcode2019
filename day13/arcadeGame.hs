
import qualified Data.HashMap.Strict as M
import Data.List
import System.IO (stdin, hSetBuffering, BufferMode(NoBuffering), hSetEcho)
import System.IO.Unsafe (unsafePerformIO)

main :: IO ()
main = do
  inp <- readFile "input2.txt"
  hSetBuffering stdin NoBuffering
  hSetEcho stdin False

  maps <- solve (readD inp)

  putStrLn (showD maps)
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


type Coord = (Integer,Integer)


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


solve :: [Integer] -> IO [String]
solve list = do

  let program = toProgram list
  let outputs = run (Computer 0 0 getInputs program)
  let (xs, ys, ts) = split outputs
  let instrs = zip (zip xs ys) ts
  let maps = scanl (\m (c,t) -> M.insert c t m) M.empty instrs

  return $ map draw (drop 1 maps)


getInputs = map (\i -> unsafePerformIO $ getOneKey i) [0..]
  where
    getOneKey :: Integer -> IO Integer
    getOneKey i = do
        key <- getChar
        case key of 
            'a' -> return (-1)
            'd' -> return 1
            _ -> return 0


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
