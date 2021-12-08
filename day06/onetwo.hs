import Text.Parsec
import Data.List

main :: IO ()
main = do
  interact (solve . readD)
  putStrLn ""

readD :: String -> [Integer]
--readD :: String -> Either ParseError [Integer]
readD s = inp
  where
    Right inp = parse readInput "" s

    readInput = parseNum `sepBy` string ","

    parseNum = do
      n <- many1 digit
      return (read n)

solve inp = unlines [show $ initstate,
                     show . sum $ states !! 80,
                     show . sum $ states !! 256]
  where
    initstate = take 9 $ geninitstate 0
      where
        geninitstate n = length (filter (==n) inp) : geninitstate (n+1)

    states = iterate nextstate initstate

    nextstate [a,b,c,d,e,f,g,h,i] = [b,c,d,e,f,g,h+a,i,a]
