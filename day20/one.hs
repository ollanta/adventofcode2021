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

showI c = showM c (:[])

solve (filter, image) = unlines $ [
  showI . snd $ states !! 0,
  showI . snd $ states !! 1,
  showI . snd $ states !! 2,
  countAt $ states !! 2,
  countAt $ states !! 50
  ]
  where
    filterm :: M.HashMap Integer Char
    filterm = M.fromList $ zip [0..] filter

    chart = readM image

    countAt (def, chart)
      | def == '#' = "Infinite + " ++ count
      | otherwise  = count
      where
        count = show . M.size . M.filter (=='#') $ chart

    states = iterate nxstate ('.', chart)

    nxstate :: (Char, M.HashMap Coord Char) -> (Char, M.HashMap Coord Char)
    nxstate (def, chart) = (def', convolved)
      where
        (minx,miny) = minimum $ M.keys chart
        (maxx,maxy) = maximum $ M.keys chart
        paddingco = concat $
          [[(minx-1, y'), (maxx+1, y')] | y' <- [miny-1..maxy+1]] ++
          [[(x', miny-1), (x', maxy+1)] | x' <- [minx-1..maxx+1]]
        padded = M.union chart . M.fromList . zip paddingco $ repeat def

        def' = nxone (replicate 9 def)
        convolved = M.mapWithKey (\k _ -> nxone (neighbours9 chart def k)) padded

    nxone str = filterm M.! (frombinary str)

neighbours9 chart def (x,y) = [M.lookupDefault def (x',y') chart |
                               y' <- [y-1..y+1],
                               x' <- [x-1..x+1]]

frombinary = fb 0
  where
    fb n ('.':s) = fb (2*n) s
    fb n ('#':s) = fb (2*n+1) s
    fb n [] = n
