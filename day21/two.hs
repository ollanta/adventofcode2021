import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M
import Parsing
import Chart2d

main :: IO ()
main = optimisticInteract parser solve

parser :: Parser [Integer]
parser = readPosition `sepEndBy` newline

readPosition = do
  string "Player " >> number >> spaces >> string "starting position: "
  pos <- number
  return pos

solve [p1,p2] = unlines $ [
  show $ ttwm1,
  show $ ttwm2,
  show $ winning1,
  show $ winning2
  ]
  where
    ttwm1 = turnstowinmap p1
    ttwm2 = turnstowinmap p2

    winning1 = sum [n1 * (nonwinning (ts1-1) ttwm2) |
                    (ts1, n1) <- M.toList ttwm1]

    winning2 = sum [n2 * (nonwinning (ts2) ttwm1) |
                    (ts2, n2) <- M.toList ttwm2]

nonwinning turn ttwmap = 27 ^ turn - winning 0 0
  where
    winning tn n
      | tn == turn = n'
      | otherwise  = winning (tn+1) n'
      where
        wt = M.lookupDefault 0 tn ttwmap
        n' = 27 * n + wt


turnstowinmap :: Integer -> M.HashMap Integer Integer
turnstowinmap pos = M.fromListWith (+) $ turnstowin pos 0 1 0


turnstowin pos s n t
  | s >= 21    = [(t,n)]
  | otherwise = [nt |
                  (nroll, roll) <- distinctRolls,
                  let pos' = step pos roll,
                  let s' = s + pos',
                  let n' = n * nroll,
                  nt <- turnstowin pos' s' n' (t+1)]


distinctRolls = map (\l -> (toInteger (length l), head l)) . group . sort $ allRolls
  where
    allRolls = [roll1 + roll2 + roll3 |
                roll1 <- [1..3],
                roll2 <- [1..3],
                roll3 <- [1..3]]

step p n
  | p' == 0   = 10
  | otherwise = p'
  where
    p' = (p+n) `mod` 10
