import Text.Parsec
import Data.List

main :: IO ()
main = do
  interact (solve . readD)
  putStrLn ""


readD :: String -> [([String], [String])]
--readD :: String -> Either ParseError ([String], [String])
readD s = inp
  where
    Right inp = parse (readInput `endBy` newline) "" s

    readInput = do
      n1 <- readStr
      string "| "
      n2 <- readStr
      return (n1, n2)

    readStr = (many1 alphaNum) `sepEndBy` string " "


orgmapping = [(0, "abcefg"),
              (1, "cf"),
              (2, "acdeg"),
              (3, "acdfg"),
              (4, "bcdf"),
              (5, "abdfg"),
              (6, "abdefg"),
              (7, "acf"),
              (8, "abcdefg"),
              (9, "abcdfg")]


transforms = genTrans "abcdefg"
  where
    genTrans [] = [[]]
    genTrans cs = [c : cs' | c <- cs, cs' <- genTrans $ filter (/=c) cs]


transform _ [] = []
transform t (c:cs) = tr c : transform t cs
  where
    tr c = snd . head . filter ((==c) . fst) $ zip "abcdefg" t

todig s = fst . head . filter ((==sort s) . snd) $ orgmapping

solve inp = unlines $ map show decoded ++ [show ans1,
                                           show ans2]
  where
    decoded = map decode inp
    
    ans1 = length . filter (`elem`[1,4,7,8]) . concat $ decoded
    ans2 = sum . map tod $ decoded

    tod [a,b,c,d] = d+10*c+100*b+1000*a

decode (codes, digs) = map todig transformeddigs
  where
    goal = sort . map snd $ orgmapping

    transformed = [(t, sort $ map (sort . transform t) codes) | t <- transforms]

    goaltransform = fst . head $ filter ((==goal) . snd) transformed

    transformeddigs = map (transform goaltransform) digs
