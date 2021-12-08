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

solve :: ([Integer], [Board]) -> String
solve (nums, boards) = unlines $ [show winningBoard,
                                  show lastNumber,
                                  show winningSum,
                                  show answer]
  where
    (lastNumber, winningBoard) = play nums boards
    winningSum = sum (map sum winningBoard)
    answer = winningSum * lastNumber

    play :: [Integer] -> [Board] -> (Integer, Board)
    play (n:ns) boards
      | any winning boards' = (n, head . filter winning $ boards')
      | otherwise = play ns boards'
      where
        boards' = remove n boards

        winning board = any ((==0) . length) board || any ((==0) . length) (transpose board)

        remove n boards = map (map (filter (/=n))) boards
