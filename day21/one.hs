import Text.Parsec
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
  show p1,
  show p2,
  showstate donestate,
  show $ calc donestate
  ] ++ map showstate (take 5 states)
  where
    rolls = cycle [1..100]

    states = iterate nextst ((p1,0), (p2,0), rolls, 0)

    done (pln, (_, s), _, _) = s >= 1000

    donestate = head . filter done $ states

    showstate (pl1, pl2, rolls, n) = show (pl1, pl2, take 3 rolls, n)

    calc ((_, s), _, _, n) = s*n

    nextst ((pos,sc),pln,rolls,nrolls) = (pln, (pos',sc'), rolls', nrolls')
      where
        nrolls' = nrolls + 3
        (roll3, rolls') = splitAt 3 rolls
        pos' = step pos (sum roll3)
        sc' = sc + pos'

step p n
  | p' == 0   = 10
  | otherwise = p'
  where
    p' = (p+n) `mod` 10
