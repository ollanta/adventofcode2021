{-# LANGUAGE FlexibleContexts #-}
import Text.Parsec
import qualified Data.HashMap.Strict as M
import Chart2d
import Parsing

main :: IO ()
main = optimisticInteract readD solve


readD :: Parser [[Char]]
readD = readRow `endBy` newline
  where
    readRow = many1 (oneOf ".v>")


solve inp = unlines [showMC $ chart,
                     unlines . map (showMC . snd) $ take 5 states,
                     showMC . snd $ steady,
                     show . fst $ steady
                    ]
  where
    chart = readM inp
    
    states = iterate step (False, chart)
    steady = head [(i, st) |
                   (i, (done, st)) <- zip [0..] states,
                   done]
    
    (maxX, maxY) = maximum . M.keys $ chart

    right (x, y) = (mod (x+1) (maxX+1), y)
    down  (x, y) = (x, mod (y+1) (maxY+1))

    step (_, m) = (doner && doned, m'')
      where
        (doner, m') = go '>' right m
        (doned, m'') = go 'v' down m'

    go c move m = (M.null moved, M.union moved m)
      where
        movers = M.filter (==c) m
        moved = M.fromList $ concat [[(pos, '.'), (move pos, c)] |
                                     pos <- M.keys movers,
                                     m M.! (move pos) == '.']
