import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Parsing
import Chart3d

main :: IO ()
main = optimisticInteract parser solve

parser :: Parser [(String,(Coord,Coord))]
parser = readCube `sepEndBy` newline

readCube = do
  onoff <- many1 letter
  spaces
  [(xs,xe),(ys,ye),(zs,ze)] <- readRange `sepBy` string ","
  return (onoff, ((xs,ys,zs), (xe,ye,ze)))

readRange = do
  oneOf "xyz" >> string "="
  s <- mnumber
  string ".."
  e <- mnumber
  return (s, e)

solve inp = unlines $ [
  show inp,
  show . sum . map size $ lights
  ]
  where
    ans = inp

    lights = foldl' combine [] inp

combine [] ("on", cube)  = [cube]
combine [] ("off", cube) = []
combine (cube:cubes) op@(_, newcube) = remove cube newcube  ++ combine cubes op


remove cube1 cube2
  | cap cube1 cube2 == Nothing = [cube1]
  | otherwise                  = remove' cube1 cube2c
  where
    Just cube2c = cap cube1 cube2

    remove' c cr = filter (/=cr) [((sx, sy, sz), (ex, ey, ez)) |
                                   (sx, ex) <- splitInterval (\(x, _, _) -> x) c cr,
                                   (sy, ey) <- splitInterval (\(_, y, _) -> y) c cr,
                                   (sz, ez) <- splitInterval (\(_, _, z) -> z) c cr,
                                   sx <= ex,
                                   sy <= ey,
                                   sz <= ez]

    splitInterval g (cs, ce) (csr, cer) = [(g cs, g csr - 1), (g csr, g cer), (g cer + 1, g ce)]

    {- faster, but harder to read
    newcubes = filter (\(s, e) -> s < e) [((xs, ys, zs), (xe, ye, zsr-1)),
                                          ((xs, ys, zer+1), (xe, ye, ze)),
                                          ((xs, ys, zsr), (xe, ysr-1, zer)),
                                          ((xs, yer+1, zsr), (xe, ye, zer)),
                                          ((xs, ysr, zsr), (xsr-1, yer, zer)),
                                          ((xer+1, ysr, zsr), (xe, yer, zer))]
        -}


size cube@(cs,ce) = (lx+1) * (ly+1) * (lz+1)
  where
    (lx, ly, lz) = sub ce cs


cap :: (Coord, Coord) -> (Coord, Coord) -> Maybe (Coord, Coord)
cap (xs1, xe1) (xs2, xe2)
  | any (<0) [lx, ly, lz] = Nothing
  | otherwise             = Just (xs', xe')
  where
    (xs', xe') = (cmap max xs1 `capp` xs2,
                  cmap min xe1 `capp` xe2)
    (lx, ly, lz) = sub xe' xs'
