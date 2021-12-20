{-# LANGUAGE FlexibleContexts #-}
import Text.Parsec
import qualified Data.HashMap.Strict as M
import Parsing
import Chart2d
import Generic
import qualified Graphics.Image as I

main :: IO ()
main = do
  content <- getContents
  let Right input = parse parser "" content
  let (minc, maxc, states) = solve input
  let images = toImages minc maxc states
  writeImages images

writeImages images = I.writeImageExact (I.Seq I.GIF) [] "image.gif" gifseq
  where
    gifseq = zip (repeat (10 :: I.GifDelay)) images

toImages minc maxc states = map toImage states
  where
    imsize = intCoord $ sub maxc minc

    toImage state = I.makeImageR I.VS imsize (toPixel state)

    intCoord (x, y) = (fromInteger x, fromInteger y)

    integerCoord (x, y) = (toInteger x, toInteger y)

    toPixel (def, chart) c
      | isLit     = pure 255 :: I.Pixel I.RGB I.Word8
      | otherwise = pure 0
      where
        --isLit = M.lookupDefault def (integerCoord c) chart == '#'
        c' = add minc (integerCoord c)
        isLit = M.lookupDefault def c' chart /= def


parser :: Parser (String, [String])
parser = do
  filter <- readFilter
  many1 newline
  image <- readImage
  return (filter, image)
  where
    readFilter = many1 (oneOf ".#")
    readImage = many1 (oneOf ".#") `sepEndBy` newline

solve (filter, image) = (minc, maxc, takeN n states)
  where
    minc = (`sub`(n,n)) . minimum . M.keys $ initchart
    maxc = (`add`(n,n)) . maximum . M.keys $ initchart

    n = 200
    filterm = M.fromList $ zip (genbins 9) filter

    count (def, chart)
      | def == '#' = ("Infinite +", c)
      | otherwise  = ("Finite", c)
      where
        c = show . M.size . M.filter (=='#') $ chart

    initchart = readM image
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
