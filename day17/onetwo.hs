import Text.Parsec
import Data.List
import Parsing


main :: IO ()
main = optimisticInteract parser solve


parser :: Parser ((Integer, Integer), (Integer, Integer))
parser = do
  string "target area: x="
  xint <- interval
  string ", y="
  yint <- interval

  return (xint, yint)
  where
    interval = do
      start <- mnumber
      string ".."
      end <- mnumber
      return (start, end)

    mnumber = do
      m <- option "" $ string "-"
      n <- many1 digit
      return $ read (m ++ n)


solve ((xstart,xend),(ystart,yend)) = unlines [
  show $ yvels,
  show $ xvels,
  "part 1:",
  show $ maxY yvelmax,
  "part 2:",
  show $ length xyvels
  ]
  where
    yvelmax = maximum . map snd $ xyvels
    maxY v = maximum . take (fromInteger $ v+1) $ genY 0 v

    xvelcands = [1..xend]

    xvels = filter hitsx xvelcands
      where
        hitsx v = any (<=xend) . dropWhile (<xstart) . dropRepeated $ genX 0 v
        dropRepeated (x1:x2:xs)
          | x1 == x2  = [x1]
          | otherwise = x1:dropRepeated (x2:xs)

    yvelcands = [-abs(xend)..abs(xend)]

    yvels = filter hitsy yvelcands
      where
        hitsy v = any (<=yend) . takeWhile (>=ystart) $ genY 0 v

    xyvelcands = [(vx,vy) | vy <- yvels, vx <- xvels]
    xyvels = filter hitsXY $ xyvelcands

    hitsXY (vx,vy) = any isin . takeWhile above $ posxy
      where
        posxy = zip (genX 0 vx) (genY 0 vy)

        above (x,y) = y >= ystart
        isin (x,y) = x >= xstart && x <= xend && y >= ystart && y <= yend


genY y v = (y+v):genY (y+v) (v-1)

genX x v = (x+v):genX (x+v) (getv v)
  where
    getv v
      | v > 0  = v-1
      | v < 0  = v+1
      | v == 0 = v
