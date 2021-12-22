import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
import Parsing
import Chart3d

main :: IO ()
main = optimisticInteract parser solve

parser :: Parser [(String,Coord,Coord)]
parser = readCube `sepEndBy` newline

readCube = do
  onoff <- many1 letter
  spaces
  [(xs,xe),(ys,ye),(zs,ze)] <- readRange `sepBy` string ","
  return (onoff, (xs,ys,zs), (xe,ye,ze))

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

    lights = foldl' turn [] inp

    turn l (op, cs, ce) = combine l op (cs, ce)

combine [] "on" cube = [cube]
combine [] "off" cube = []
combine (cube:more) op newcube = minus cube newcube  ++ combine more op newcube

corners cube@(cs, ce) = [(x,y,z) |
                         let (xs, ys, zs) = cs,
                         let (xe, ye, ze) = ce,
                         x <- [xs, xe],
                         y <- [ys, ye],
                         z <- [zs, ze]]

isin (x,y,z) ((xs,ys,zs), (xe,ye,ze)) = x >= xs && x <= xe && y >= ys && y <= ye && z >= zs && z <= ze

overlaps cube1@(cs1, ce1) cube2@(cs2, ce2) = xs<=xe && ys<=ye && zs<=ze
  where
    ((xs,ys,zs),(xe,ye,ze)) = cap cube1 cube2
{-  any (`isin` cube2) (corners cube1) ||
  any (`isin` cube1) (corners cube2) ||-}
  
  
isfullyin cube1 cube2 = all (`isin` cube2) (corners cube1)

minus cube1@(cs1, ce1) cube2@(cs2, ce2)
  | overlaps cube1 cube2c = filter (not . (`isfullyin` cube2)) splitcubes
  | otherwise = [cube1]
  where
    cube2c = cap cube1 cube2
    splitcubes = remove cube1 cube2c


remove cube1@(cs, ce) cuber@(csr, cer) = filter correct newcubes
  where
    (xs, ys, zs) = cs
    (xe, ye, ze) = ce
    (xsr, ysr, zsr) = csr
    (xer, yer, zer) = cer
    
    newcubes :: [(Coord, Coord)]
    newcubes = [(cs, (xsr-1, ysr-1, zsr-1)),

                ((xsr,ys,zs), (xe, ysr-1, zsr-1)),
                ((xs,ysr,zs), (xsr-1, ye, zsr-1)),
                ((xs,ys,zsr), (xsr-1, ysr-1, ze)),

                ((xsr,ysr,zs), (xe, ye, zsr-1)),
                ((xsr,ys,zsr), (xe, ysr-1, ze)),
                ((xs,ysr,zsr), (xsr-1, ye, ze)),

                --(csr, cer) <- removed,

                ((xer+1,ysr,zsr), (xe, yer, zer)),
                ((xsr,yer+1,zsr), (xer, ye, zer)),
                ((xsr,ysr,zer+1), (xer, yer, ze)),

                ((xer+1,yer+1,zsr), (xe, ye, zer)),
                ((xer+1,ysr,zer+1), (xe, yer, ze)),
                ((xsr,yer+1,zer+1), (xer, ye, ze)),

                ((xer+1, yer+1, zer+1), ce)
                ]
    correct ((xs,ys,zs),(xe,ye,ze)) = xs <= xe && ys <= ye && zs <= ze


size cube@(cs,ce) = (lx+1) * (ly+1) * (lz+1)
  where
    (lx, ly, lz) = sub ce cs


cap :: (Coord, Coord) -> (Coord, Coord) -> (Coord, Coord)
cap ((xs,ys,zs), (xe,ye,ze)) ((xs1,ys1,zs1), (xe1,ye1,ze1)) =
  ((max xs1 xs, max ys1 ys, max zs1 zs),
   (min xe1 xe, min ye1 ye, min ze1 ze))
