{-# LANGUAGE FlexibleContexts #-}
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


solve inp = unlines $ [showMC $ inpchart] ++ map show (takeEvery 1000 ans)
  where
    inpchart = readM inp

    ans = search inpchart

    search ichart = search' iheap M.empty
      where
        iheap :: H.MinPrioHeap Integer (M.HashMap Coord Char)
        iheap = H.singleton (0, ichart)

        search' prioh visited
          | (showMC chart) `M.member` visited = search' prioh' visited'
          | done chart = [cost]
          | otherwise = cost:search' prioh'' visited'
          where
            ([(cost, chart)], prioh') = H.splitAt 1 prioh
            visited' = M.insert (showMC chart) True visited

            newstates = H.fromList [(cost+mcost, chart') |
                                    (mcost, chart') <- genMoves chart]
            prioh'' = H.union prioh' newstates

    toStrings = lines . showMC

takeEvery n l
  | null l'   = [head l, last l]
  | otherwise = head l : takeEvery n l'
  where
    (_, l') = splitAt n l

done ch = and [c == ch M.! (getXfor c, y) |
               y <- [2..maxY-1],
               c <- "ABCD"]
  where
    maxY = maximum . map snd . M.keys $ ch


genMoves :: M.HashMap Coord Char -> [(Integer, M.HashMap Coord Char)]
genMoves chart
  | null movesin = movesout
  | otherwise    = take 1 movesin
  where
    amphipods = M.filter (`elem` "ABCD") chart
    corridor = [(x,1) | x <- [1..11], not (x `elem` [3,5,7,9])]

    move :: Coord -> Coord -> M.HashMap Coord Char
    move start end = M.union upd chart
      where
        upd = M.fromList [(start, '.'), (end, chart M.! start)]

    movesin = clean [(c, start, end) |
                     (start, c) <- M.toList amphipods,
                     getY start == 1,
                     end <- wantsIn chart c]

    movesout = clean [(c, start, end) |
                      (start, c) <- M.toList amphipods,
                      wantsOut chart start c,
                      end <- corridor,
                      getY start >= 2]

    clean moves = [(cost c path, move start end) |
                   (c, start, end) <- moves,
                   let path = tail (genpath start end),
                   all (=='.') [chart M.! pos | pos <- path]]

    genpath s@(sx,sy) e@(ex,ey)
      | s == e = [e]
      | sx == ex = s : genpath (sx, sy+signum (ey-sy)) e
      | sy >= 2  = s : genpath (sx, sy+signum (1-sy)) e
      | otherwise = s : genpath (sx+signum (ex-sx), sy) e

wantsOut chart (x,y) c
  | any (/='.') [chart M.! (x,y') | y' <- [2..y-1]] = False
  | getXfor c /= x = True
  | otherwise = any (/=c) [chart M.! (x,y') | y' <- [y+1..maxY-1]]
  where
    maxY = maximum . map snd . M.keys $ chart

wantsIn chart c
  | chart M.! pos /= '.' = []
  | otherwise  = [pos]
  where
    maxY = maximum . map snd . M.keys $ chart
    pos = head $ dropWhile (\p -> chart M.! p == c) [(getXfor c, y) | y <- reverse [2..maxY-1]]

getXfor 'A' = 3
getXfor 'B' = 5
getXfor 'C' = 7
getXfor 'D' = 9

cost 'A' path = 1 * lengthN path
cost 'B' path = 10 * lengthN path
cost 'C' path = 100 * lengthN path
cost 'D' path = 1000 * lengthN path

lengthN = toInteger . length

