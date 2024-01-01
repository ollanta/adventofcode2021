
funs = [f 13 0,
        f 11 3,
        f 14 8,
        g (-5) 5,
        f 14 13,
        f 10 9,
        f 12 6,
        g (-14) 1,
        g (-8) 1,
        f 13 2,
        g 0 7,
        g (-5) 5,
        g (-9) 8,
        g (-1) 15]

calc inp = foldl (\z (f, i) -> f i z) 0 (zip funs inp)

f a b w z
  | x /= w = 26*z + y
  | x == w = z
  where
    x = mod z 26 + a
    y = (w + b)

g a b w z
  | x /= w = 26*z' + y
  | x == w = z'
  where
    x = mod z 26 + a
    z' = div z 26
    y = (w + b)
