import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M
import Chart2d
import Parsing

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


    search chart = search' [(0, (0,0))] M.empty
      where
        goal = maximum $ M.keys chart

        search' ((rsk, co):rest) visited
          | co == goal = rsk
          | otherwise  = search' rest''' visited'
          where
            visited' = M.insert co True visited

            neigh = filter (`M.member` chart) $ neighbours co
            rest' = rest ++ map (\c -> (rsk + chart M.! c, c)) neigh
            rest'' = filter (\(_,c) -> not (M.member c visited)) rest'
            rest''' = sort rest''
