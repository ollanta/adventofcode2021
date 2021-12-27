import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M
import Chart2d
import Parsing
import qualified Data.Heap as H

main :: IO ()
main = optimisticInteract readD solve


readD :: Parser [[Char]]
readD = readRow `endBy` newline
  where
    readRow = many1 (oneOf "#. ABCD")


solve inp = unlines $ [showMC (readM inp)] ++ map show (takeEvery 1000 ans)
  where
    amphipods = M.filter (`elem` "ABCD") $ readM inp
    maxY = maximum . map snd . M.keys $ amphipods

    ans = search amphipods

    search poschart = search' iheap M.empty
      where
        iheap :: H.MinPrioHeap Integer (M.HashMap Coord Char)
        iheap = H.singleton (0, poschart)

        toHashable = sort . M.toList

        search' prioh visited
          | (toHashable posch) `M.member` visited = search' prioh' visited'
          | done posch = [cost]
          | otherwise = cost:search' prioh'' visited'
          where
            ([(cost, posch)], prioh') = H.splitAt 1 prioh
            visited' = M.insert (toHashable posch) True visited

            newstates = H.fromList [(cost+mcost, posch') |
                                    (mcost, posch') <- genMoves maxY posch]
            prioh'' = H.union prioh' newstates

takeEvery n l
  | null l'   = [head l, last l]
  | otherwise = head l : takeEvery n l'
  where
    (_, l') = splitAt n l

done ch = and [x == getXfor c |
               ((x,_), c) <- M.toList ch]

genMoves :: Integer -> M.HashMap Coord Char -> [(Integer, M.HashMap Coord Char)]
genMoves maxY chart
  | null movesin = movesout
  | otherwise    = take 1 movesin
  where
    corridor = [(x,1) | x <- [1..11], not (x `elem` [3,5,7,9])]

    move :: Coord -> Coord -> M.HashMap Coord Char
    move start end = chart''
      where
        chart' = M.delete start chart
        chart'' = M.insert end (chart M.! start) chart'

    movesin = clean [(c, start, end) |
                     (start, c) <- M.toList chart,
                     getY start == 1,
                     end <- wantsIn maxY chart c]

    movesout = clean [(c, start, end) |
                      (start, c) <- M.toList chart,
                      wantsOut maxY chart start,
                      end <- corridor,
                      getY start >= 2]

    clean moves = [(cost c * lengthN path, move start end) |
                   (c, start, end) <- moves,
                   let path = tail (genpath start end),
                   not . any (`M.member` chart) $ path]

    genpath s@(sx,sy) e@(ex,ey)
      | s == e = [e]
      | sx == ex = s : genpath (sx, sy+signum (ey-sy)) e
      | sy >= 2  = s : genpath (sx, sy+signum (1-sy)) e
      | otherwise = s : genpath (sx+signum (ex-sx), sy) e

wantsOut maxY ch (x,y)
  | not canOut = False
  | otherwise = not isDone
  where
    canOut = all (==False) [M.member (x,y') ch | y' <- [2..y-1]]
    isDone = all (==True) [x == getXfor c |
                           y' <- [y..maxY],
                           let c = ch M.! (x,y')
                           ]
    

wantsIn maxY ch c
  | head pos `M.member` ch = []
  | otherwise  = take 1 pos
  where
    pos = [(x, y) |
           y <- [maxY,maxY-1..2],
           let x = getXfor c,
           M.lookupDefault '.' (x, y) ch /= c]


getXfor 'A' = 3
getXfor 'B' = 5
getXfor 'C' = 7
getXfor 'D' = 9

cost 'A' = 1
cost 'B' = 10
cost 'C' = 100
cost 'D' = 1000

lengthN = toInteger . length

