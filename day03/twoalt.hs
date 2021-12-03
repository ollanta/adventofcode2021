import Data.List

main :: IO ()
main = interact run
  where run = solve . lines

data RType = Most | Least
  deriving (Eq)

solve :: [String] -> String
solve inps = unlines [oxygen, cotwo, show ans]
  where
    oxygen = search Most (sort inps)
    cotwo = search Least (sort inps)

    ans = frombinary oxygen * frombinary cotwo

    search _ [bin] = bin
    search rt bins = next rt : search rt (rest $ next rt)
      where
        median = bins !! div (length bins) 2
        mc = head median

        next Most = mc
        next Least = inv mc

        (zeroes, ones) = span ((=='0') . head) bins
        rest '0' = map (drop 1) zeroes
        rest '1' = map (drop 1) ones

    inv '0' = '1'
    inv '1' = '0'

frombinary = fb 0
  where
    fb n ('0':s) = fb (2*n) s
    fb n ('1':s) = fb (2*n+1) s
    fb n [] = n
