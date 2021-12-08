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
solve crabs = unlines $ map show [medcrab, fuelcost medcrab]
  where
    crabs' = sort crabs
    lnc = toInteger $ length crabs

    medcrab = crabs' !! fromInteger (lnc `div` 2)

    fuelcost p = sum . map (abs . (p-)) $ crabs
