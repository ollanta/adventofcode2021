
main :: IO ()
main = interact run
  where run = solve . lines

solve :: [String] -> String
solve ss = unlines [gamma, epsilon, show ans]
  where
    gamma = solve' ss
    epsilon = invert gamma

    ans = frombinary gamma * frombinary epsilon
    
    solve' ls
      | (length . head) ls == 0 = []
      | otherwise = mc : solve' (map (drop 1) ls)
      where
        cs = map head ls
        zeroes = (length . filter (=='0')) cs
        ones = (length . filter (=='1')) cs

        mc = if zeroes > ones then '0' else '1'

invert = map flipbit
  where
    flipbit '0' = '1'
    flipbit '1' = '0'

frombinary = fb 0
  where
    fb n ('0':s) = fb (2*n) s
    fb n ('1':s) = fb (2*n+1) s
    fb n [] = n
