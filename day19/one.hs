import Text.Parsec
import Data.Function
import Data.List
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Parsing

main :: IO ()
main = optimisticInteract parser solve


data Coord3 = C Integer Integer Integer
  deriving (Eq, Show, Ord)

parser :: Parser [(Integer, [Coord3])]
parser = readScanner `sepEndBy` newline
  where

readScanner = do
  string "--- scanner "
  n <- number
  string " ---"
  newline
  cs <- readCoord `sepEndBy` newline
  return (n, cs)

readCoord = do
  x <- mnumber
  string ","
  y <- mnumber
  string ","
  z <- mnumber
  return $ C x y z


solve inp = unlines $ [
  show $ inp,
  show $ M.keys puzzled,
  show $ coords,
  show $ S.size coords,
  show scannerpos,
  show largestdist
  ]
  where
    coords = S.unions . map (toSet . snd) $ M.elems puzzled
    scannerpos = map fst $ M.elems puzzled
    largestdist = maximum [manhattan sc1 sc2 | sc1 <- scannerpos, sc2 <- scannerpos]

    puzzled = puzzle inp

    puzzle ((n,cs):scanners) = puzzle' M.empty (M.singleton n (C 0 0 0,cs)) (M.fromList scanners)
      where
        puzzle' oldm newm scanmap
          | M.null newm = oldm
          | otherwise   = puzzle' (M.union oldm newm) newm' scanmap'
          where
            overlapped = [(sn, ovrl) |
                          (sn, scs) <- M.toList scanmap,
                          (_ ,(_, kcs)) <- M.toList newm,
                          let ovrl = overlaps kcs scs]

            newm' = M.fromList [(on, (pos, cs')) |
                                (on, Just (pos, cs')) <- overlapped]
            scanmap' = foldr M.delete scanmap (M.keys newm')

manhattan (C x1 y1 z1) (C x2 y2 z2) = abs (x1-x2) + abs (y1-y2) + abs (z1-z2)

overlaps cs1 cs2
  | null overlapping = Nothing
  | otherwise        = Just (head overlapping)
  where
    cs1s = toSet cs1

    rsc = [(rot, map rot cs2) | rot <- rotations24]
    diffs = [(rot, sub m1c m2c, cs2c) | m1c <- cs1, (rot, cs2c) <- rsc, m2c <- cs2c]

    rsc' = [(diff, map (add diff) cs) | (rot, diff, cs) <- diffs]

    overlapping = [(diff, cs) | (diff, cs) <- rsc',
                    S.size (S.intersection cs1s (toSet cs)) >= 12]

toSet cs = S.fromList . map (\(C x y z) -> (x,y,z)) $ cs

rotations24 = nubBy ((==) `on` ($ C 1 2 3)) rotations64

rotations64 = [rot |
               rx <- take 4 (iterate (rotX.) id),
               ry <- take 4 (iterate (rotY.) id),
               rz <- take 4 (iterate (rotZ.) id),
               let rot = rx . ry . rz]


rotX (C x y z) = C x (-z) y
rotY (C x y z) = C (-z) y x
rotZ (C x y z) = C y (-x) z

add (C x1 y1 z1) (C x2 y2 z2) = C (x1+x2) (y1+y2) (z1+z2)

sub (C x1 y1 z1) (C x2 y2 z2) = C (x1-x2) (y1-y2) (z1-z2)
