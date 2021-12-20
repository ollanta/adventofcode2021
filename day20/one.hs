import Text.Parsec
import qualified Data.HashMap.Strict as M
import Parsing
import Chart2d

main :: IO ()
main = optimisticInteract parser solve

parser :: Parser (String, [String])
parser = do
  filter <- readFilter
  many1 newline
  image <- readImage
  return (filter, image)
  where
    readFilter = many1 (oneOf ".#")

    readImage = many1 (oneOf ".#") `sepEndBy` newline

solve (filter, image) = unlines $ [
  showMC . snd $ states !! 0,
  showMC . snd $ states !! 1,
  showMC . snd $ states !! 2,
  show . count $ states !! 2,
  show . count $ states !! 50
  ]
  where
    filterm = M.fromList $ zip (genbins 9) filter

    count (def, chart)
      | def == '#' = ("Infinite +", c)
      | otherwise  = ("Finite", c)
      where
        c = show . M.size . M.filter (=='#') $ chart

    states = iterate nxstate ('.', readM image)

    nxstate :: (Char, M.HashMap Coord Char) -> (Char, M.HashMap Coord Char)
    nxstate (def, chart) = (def', convolved)
      where
        (minx,miny) = minimum $ M.keys chart
        (maxx,maxy) = maximum $ M.keys chart
        paddingco = concat $
          [[(minx-1, y'), (maxx+1, y')] | y' <- [miny-1..maxy+1]] ++
          [[(x', miny-1), (x', maxy+1)] | x' <- [minx-1..maxx+1]]
        padded = M.union chart . M.fromList . zip paddingco $ repeat def

        def' = filterm M.! replicate 9 def
        convolved = M.mapWithKey (\c _ -> filterm M.! neighbours9 chart def c) padded

neighbours9 chart def (x,y) = [M.lookupDefault def (x',y') chart |
                               y' <- [y-1..y+1],
                               x' <- [x-1..x+1]]

genbins 0 = [[]]
genbins n = [b:rest | b <- ".#", rest <- genbins (n-1)]
