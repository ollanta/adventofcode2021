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
  show . calcSize 0 $ lights2
  ]
  where
    ans = inp

    lights2 = foldl' combine [] inp

cap :: (Coord, Coord) -> (Coord, Coord) -> Maybe (Coord, Coord)
cap (xs1, xe1) (xs2, xe2)
  | any (<0) [lx, ly, lz] = Nothing
  | otherwise             = Just (xs', xe')
  where
    (xs', xe') = (cmap max xs1 `capp` xs2,
                  cmap min xe1 `capp` xe2)
    (lx, ly, lz) = sub xe' xs'


combine [] ("on", cube) = [("on", cube)]
combine [] ("off", cube) = []
combine ((op, cube):cubes) (nop, newcube) = case cap cube newcube of
  Just icube -> [(op, cube), (inv op, icube)] ++ cont
  Nothing    -> [(op, cube)] ++ cont
  where
    cont = combine2 cubes (nop, newcube)

    inv "on" = "off"
    inv "off" = "on"
    

calcSize n (("on", cube):cubes) = calcSize (n+size cube) cubes
calcSize n (("off", cube):cubes) = calcSize (n-size cube) cubes
calcSize n [] = n
