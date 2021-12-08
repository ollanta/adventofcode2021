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

breakup l@(c1, c2) = fromTo c1 c2

fromTo c1@(C x1 y1) c2@(C x2 y2)
  | c1 == c2  = [c1]
  | otherwise = c1 : fromTo nc c2
  where
    dx = signum $ x2-x1
    dy = signum $ y2-y1
    nc = C (x1+dx) (y1+dy)

solve lines = show ans
  where
    allcoords = concat . map breakup $ lines

    groups = group . sort $ allcoords

    groups' = filter ((>1) . length) groups

    ans = length groups'
