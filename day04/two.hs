import Text.Parsec
import Data.List

main :: IO ()
main = do
  interact (solve . readD)
  putStrLn ""

type Board = [[Integer]]

readD :: String -> ([Integer], [Board])
--readD :: String -> Either ParseError ([Integer], [Board])
readD s = inp
  where
    Right inp = parse readInput "" s

    readInput = do
      ns <- (many1 digit) `sepBy1` string ","
      newline
      bs <- readBoard `sepBy1` newline
      return (map read ns, bs)

    readBoard = (parseNum `sepBy1` string " ") `endBy1` newline

    parseNum = do
      skipMany space
      n <- many1 digit
      return (read n)

data Point = P Bool Integer

gone (P b _) = b

int (P True _) = 0
int (P _ i)    = i

toPoint = P False

mapBoard f b = map (map f) b

solve :: ([Integer], [Board]) -> String
solve (nums, boards) = unlines $ [show winningBoard,
                                  show lastNumber,
                                  show winningSum,
                                  show answer]
  where
    (lastNumber, winningBoard) = play nums (map (mapBoard toPoint) boards)
    winningSum = sum (map sum winningBoard)
    answer = winningSum * lastNumber

    play :: [Integer] -> [ [[Point]] ] -> (Integer, Board)
    play (n:ns) boards
      | all winning boards' = (n, toBoard $ head boards')
      | otherwise = play ns boards''
      where
        boards' = remove n boards
        boards'' = filter (not . winning) boards'

        winning board = any (all gone) board || any (all gone) (transpose board)

        remove n boards = map (mapBoard (remove' n)) boards
          where
            remove' n (P False i)
              | n == i = P True i
            remove' n p = p

        toBoard board = map (map int) board
