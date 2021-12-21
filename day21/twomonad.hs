{-# LANGUAGE FlexibleContexts #-}
import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M
import Parsing
import Control.Applicative
import Control.Monad

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
    player1, player2 :: DProb (Integer, Integer)
    player1 = pure (pos1, 0)
    player2 = pure (pos2, 0)

    winCount player@(_, w) = w
    ongoingGames player@(m, _) = m

    states = iterate play ((player1, 0), (player2, 0))
    finalState = head . filter ((==0) . countStates . fst . snd) $ states

    play (p1, p2) = (p2, p1')
      where
        p1' = (contGames, winCount p1 + newWins)

        gameStates = reduce (add <$> ongoingGames p1 <*> allRolls)
        add (pos, score) roll = let pos' = step pos roll in (pos', score+pos')

        winningGames = mfilter ((>=21) . snd) gameStates
        contGames = mfilter ((<21) . snd) gameStates

        newWins = countStates $ ongoingGames p2 >> winningGames

    allRolls :: DProb Integer
    allRolls = d3 <+> d3 <+> d3

d3 = DProb [(i, 1) | i <- [1..3]]

step p n
  | p' == 0   = 10
  | otherwise = p'
  where
    p' = (p+n) `mod` 10


data DProb a = DProb [(a, Integer)]
  deriving (Show, Eq)

instance Functor DProb  where
  fmap f (DProb pl) = DProb [(f s, c) | (s, c) <- pl]

instance Applicative DProb where
  pure a = DProb [(a, 1)]
  (<*>) (DProb fpl) (DProb pl) = DProb [(f s, fc * sc) |
                                        (f, fc) <- fpl,
                                        (s, sc) <- pl]

instance Alternative DProb where
  empty = DProb []
  (<|>) (DProb pl1) (DProb pl2) = DProb $ pl1 ++ pl2

instance Monad DProb where
  (>>=) (DProb pl) f = DProb [(fs, fc * sc) |
                              (s, sc) <- pl,
                              (fs, fc) <- inspect $ f s]

instance MonadPlus DProb

inspect (DProb pl) = pl
reduce (DProb pl) = DProb . M.toList . M.fromListWith (+) $ pl
countStates (DProb pl) = sum . map snd $ pl

(<+>) a b = reduce (liftA2 (+) a b)
