
main :: IO ()
main = interact run
  where run = solve . lines

solve :: [String] -> String
solve ss = unlines [oxygen, cotwo, show ans]
  where
    oxygen = solve' (>) ss
    cotwo = solve' (<=) ss

    ans = frombinary oxygen * frombinary cotwo
    
    solve' cmp ls
      | (length . head) ls == 0 = []
      | length ls == 1 = head ls
      | otherwise = mc : solve' cmp nextls
      where
        cs = map head ls
        zeroes = (length . filter (=='0')) cs
        ones = (length . filter (=='1')) cs

        mc = if cmp zeroes ones then '0' else '1'

        nextls = map (drop 1) . (filter ((==) mc . head)) $ ls

frombinary = fb 0
  where
    fb n ('0':s) = fb (2*n) s
    fb n ('1':s) = fb (2*n+1) s
    fb n [] = n
