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
  show ans
  ]
  where
    ans :: Int
    ans = S.size . S.filter within50 $ lights

    within50 (x,y,z) = abs(x) <= 50 && abs(y) <= 50 && abs(z) <= 50

    initst :: S.HashSet Coord
    initst = S.empty

    lights = foldl' turn initst inp

    turn s (op, cs, ce)
      | op == "on"  = S.union s cube
      | op == "off" = S.difference s cube
      where
        (xs, ys, zs) = cmap cap cs
        (xe, ye, ze) = cmap cap ce
        cube = (S.fromList [(x,y,z) | x <- [xs..xe], y <- [ys..ye], z <- [zs..ze]])

cap n
  | n <= -51  = -51
  | n >= 51   = 51
  | otherwise = n
