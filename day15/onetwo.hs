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


    search chart = search' ((0,0),0) M.empty M.empty
      where
        goal = maximum $ M.keys chart

        search' (co, rsk) shortmap visited
          | co == goal = rsk
          | otherwise  = search' next shortmap'' visited'
          where
            visited' = M.insert co True visited

            neigh = filter (\c -> M.member c chart && not (M.member c visited)) $ neighbours co
            shortmap' = M.unionWith min shortmap $ M.fromList (map (\c -> (c, rsk + chart M.! c)) neigh)
            shortmap'' = M.delete co shortmap'
            next = flip . minimum . map flip $ M.toList shortmap''

            flip (a,b) = (b,a)
