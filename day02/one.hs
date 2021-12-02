
main :: IO ()
main = interact run
  where run = show . solve . map read' . lines
        read' s = (s', read s'')
          where (s', s'') = span (/=' ') s
          

solve :: [(String, Integer)] -> Integer
solve l = solve' (0, 0) l
  where
    solve' pd (l:ls) = solve' (apply pd l) ls
    solve' (p, d) [] = p*d

    apply (p, d) ("forward", n) = (p+n, d)
    apply (p, d) ("down", n) = (p, d+n)
    apply (p, d) ("up", n) = (p, d-n)
