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

solve [p1,p2] = show . scores $ donestate
  where
    states = iterate nextst (M.singleton (p1,0) 1, 0, M.singleton (p2,0) 1, 0)

    done (_, _, m2, _) = M.null m2

    donestate = head . filter done $ states

    scores (_, s1, _, s2) = (s1,s2)

    nextst (m1, w1, mn, wn) = (mn, wn, m1c, w1')
      where
        m1' = M.fromListWith (+) [((p', s+p'), c*n) |
                                  ((p, s), c) <- M.toList m1,
                                  (n, roll) <- distinctRolls,
                                  let p' = step p roll]
        m1c = M.filterWithKey (\(_, s) _ -> s < 21) m1'
        m1w = M.filterWithKey (\(_, s) _ -> s >= 21) m1'
        games = sum . M.elems $ mn
        w1'  = (w1+) . (games*) . sum . M.elems $ m1w


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
