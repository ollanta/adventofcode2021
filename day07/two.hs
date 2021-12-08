import Text.Parsec
import Data.List

main :: IO ()
main = do
  interact (solve . readD)
  putStrLn ""


readD :: String -> [Integer]
--readD :: String -> Either ParseError [Integer])
readD s = inp
  where
    Right inp = parse readInput "" s

    readInput = do
      ns <- (many1 digit) `sepBy1` string ","
      return (map read ns)

solve :: [Integer] -> String
solve crabs = unlines . map show . take 3 . sort $ crabcosts
  where
    crabs' = sort crabs

    fuelcost p = sum . map (cost p) $ crabs

    mincrab = head crabs'
    maxcrab = last crabs

    crabcosts = [(fuelcost pos, pos) | pos <- [mincrab..maxcrab]]
    
cost :: Integer -> Integer -> Integer
cost p0 p1 = cost' $ abs (p1 - p0)
  where
    cost' d = d * (d+1) `div` 2
