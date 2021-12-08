import Text.Parsec
import Data.List

main :: IO ()
main = do
  interact (solve . readD)
  putStrLn ""

data Coord = C Integer Integer
  deriving (Show, Eq, Ord)

type CLine = (Coord, Coord)

readD :: String -> [CLine]
--readD :: String -> Either ParseError [CLine]
readD s = inp
  where
    Right inp = parse readInput "" s

    readInput = readLine `endBy` newline

    readLine = do
      c1 <- readCoord
      string " -> "
      c2 <- readCoord
      return (c1, c2)
      
    readCoord = do
      x <- parseNum
      string ","
      y <- parseNum
      return $ C x y

    parseNum = do
      skipMany space
      n <- many1 digit
      return (read n)

perp l = horiz l || vert l

horiz ((C _ y1), (C _ y2)) = y1 == y2

vert ((C x1 _), (C x2 _))  = x1 == x2

breakup l@((C x1 y1), (C x2 y2))
  | horiz l = [C x y1 | x <- [x1..x2] ++ [x2..x1]]
  | vert l  = [C x1 y | y <- [y1..y2] ++ [y2..y1]]

solve lines = show ans
  where
    lines' = filter perp lines

    allcoords = concat . map breakup $ lines'

    groups = group . sort $ allcoords

    groups' = filter ((>1) . length) groups

    ans = length groups'
