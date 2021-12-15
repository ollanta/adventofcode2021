import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M
import Chart2d
import Parsing
import qualified Data.Heap as H

main :: IO ()
main = optimisticInteract readD solve


readD :: Parser [[Integer]]
readD = readRow `endBy` newline
  where
    readRow = many1 digitAsNumber


solve inp = unlines [showMS $ chart,
                     show $ search chart,
                     show $ search chart']
  where
    chart' = duplicate 5 chart
    answer = search chart'
    chart = readM inp

    duplicate n ch = M.unions $ [helper dx dy | dx <- [0..n-1], dy <- [0..n-1]]
      where
        mx = (1+) . maximum . map fst $ M.keys ch
        my = (1+) . maximum . map snd $ M.keys ch

        helper dx dy = M.fromList [((x+dx*mx, y+dy*my), cap (r+dx+dy)) | ((x,y),r) <- M.toList ch]

    cap r
      | r > 9     = r - 9
      | otherwise = r


    search chart = search' initheap M.empty
      where
        goal = maximum $ M.keys chart

        initheap :: H.MinPrioHeap Integer Coord
        initheap = H.singleton (0, (0,0))

        search' prioh visited
          | co `M.member` visited = search' prioh' visited
          | co == goal = rsk
          | otherwise  = search' prioh'' visited'
          where
            ([(rsk, co)], prioh') = H.splitAt 1 prioh
            visited' = M.insert co True visited

            neighs = [c | c <- neighbours co, M.member c chart, not (M.member c visited)]
            neighheap = H.fromList [(rsk + chart M.! c, c) | c <- neighs]

            prioh'' = H.union prioh' neighheap
