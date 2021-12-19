import Text.Parsec
import Data.Function
import Data.List
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Parsing
import Chart3d

main :: IO ()
main = optimisticInteract parser solve

parser :: Parser [(Integer, [Coord])]
parser = readScanner `sepEndBy` newline
  where

readScanner = do
  n <- string "--- scanner " *> number <* string " ---"
  newline
  cs <- readCoord `sepEndBy` newline
  return (n, cs)

readCoord = do
  [x,y,z] <- mnumber `sepBy` string ","
  return $ (x, y, z)

solve inp = unlines $ [
  show $ S.size coords,
  show largestdist
  ]
  where
    coords = S.unions . map (S.fromList . snd) $ M.elems puzzled
    scannerpos = map fst $ M.elems puzzled
    largestdist = maximum [manhattan sc1 sc2 | sc1 <- scannerpos, sc2 <- scannerpos]

    puzzled = puzzle inp

    puzzle ((n,cs):scanners) = puzzle' M.empty (M.singleton n ((0, 0, 0),cs)) (M.fromList scanners)
      where
        puzzle' oldm newm scanmap
          | M.null newm = oldm
          | otherwise   = puzzle' (M.union oldm newm) newm' scanmap'
          where
            overlapped = [(sn, overlaps kcs scs) |
                          (sn, scs) <- M.toList scanmap,
                          (_, kcs) <- M.elems newm]

            newm' = M.fromList [(sn, (pos, scs')) |
                                (sn, Just (pos, scs')) <- overlapped]
            scanmap' = foldr M.delete scanmap (M.keys newm')

manhattan c1 c2 = norm2 $ sub c1 c2

overlaps cs1 cs2
  | null overlapping = Nothing
  | otherwise        = Just (head overlapping)
  where
    cs1s = S.fromList cs1

    rsc = [(rot, map rot cs2) | rot <- rotations]
    diffs = [(rot, sub m1c m2c, cs2c) | m1c <- cs1, (rot, cs2c) <- rsc, m2c <- cs2c]

    rsc' = [(diff, map (add diff) cs) | (rot, diff, cs) <- diffs]

    overlapping = [(diff, cs) | (diff, cs) <- rsc',
                    S.size (S.intersection cs1s (S.fromList cs)) >= 12]
