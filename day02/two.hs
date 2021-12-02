
main :: IO ()
main = interact run
  where run = show . solve . map read' . lines
        read' s = (s', read s'')
          where (s', s'') = span (/=' ') s
          

solve :: [(String, Integer)] -> Integer
solve l = solve' (0, 0, 0) l
  where
    solve' pda (l:ls) = solve' (apply pda l) ls
    solve' (p, d, a) [] = p*d

    apply (p, d, a) ("forward", n) = (p+n, d+a*n, a)
    apply (p, d, a) ("down", n) = (p, d, a+n)
    apply (p, d, a) ("up", n) = (p, d, a-n)
