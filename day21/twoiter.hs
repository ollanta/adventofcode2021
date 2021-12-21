import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M
import Parsing
import Chart2d
import Control.Applicative

main :: IO ()
main = optimisticInteract parser solve

parser :: Parser [Integer]
parser = readPosition `sepEndBy` newline

readPosition = do
  string "Player " >> number >> spaces >> string "starting position: "
  pos <- number
  return pos

solve [pos1,pos2] = show finalState
  where
    player1, player2 :: (M.HashMap (Integer, Integer) Integer, Integer)
    player1 = (M.singleton (pos1, 0) 1, 0)
    player2 = (M.singleton (pos2, 0) 1, 0)

    winCount player@(_, w) = w
    ongoingGames player@(m, _) = m

    states = iterate play (player1, player2)
    finalState = head . filter (M.null . ongoingGames . snd) $ states

    play (p1, p2) = (p2, p1')
      where
        newStates = M.fromListWith (+) [((pos', score+pos'), count * rollCount) |
                                        ((pos, score), count) <- M.toList . ongoingGames $ p1,
                                        (roll, rollCount) <- distinctRolls,
                                        let pos' = step pos roll]
        winningGames = filterScore (>=21) newStates
        otherGames = filterScore (<21) newStates

        newWins = countGames winningGames * countGames (ongoingGames p2)
        p1' = (otherGames, winCount p1 + newWins)

    countGames gameState = sum . M.elems $ gameState

    filterScore f gameState = M.filterWithKey (\(_, score) _ -> f score) gameState

distinctRolls = [(head l, toInteger (length l)) | l <- group . sort $ allRolls]
  where
    (<+>) = liftA2 (+)
    allRolls = [1..3] <+> [1..3] <+> [1..3]

step p n
  | p' == 0   = 10
  | otherwise = p'
  where
    p' = (p+n) `mod` 10
