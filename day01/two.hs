
main :: IO ()
main = interact run
  where run = show . solve . map read . lines

solve :: [Integer] -> Integer
solve l = solve' 0 (smooth l)
  where
    solve' n (a:b:is)
      | b > a = solve' (n+1) (b:is)
      | otherwise = solve' n (b:is)
    solve' n _ = n

    smooth (a:b:c:ls) = a+b+c:smooth (b:c:ls)
    smooth _ = []
