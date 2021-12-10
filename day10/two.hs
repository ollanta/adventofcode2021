import Text.Parsec
import Data.List


main :: IO ()
main = do
  interact (solve . lines)
  putStrLn ""


solve inps = show $ answer
  where
    scores = map (score . fromJust) . filter (/=Nothing) . map syntaxerror $ inps

    answer = (!! (length scores `div` 2)) . sort $ scores

    syntaxerror (c:cs) = se [] c cs
      where
        se prev last (next:rest)
          | next `elem` opening = se (last:prev) next rest
          | next == close last  = case null prev of
              True -> se [] (head rest) (tail rest)
              False -> se (drop 1 prev) (head prev) rest
          | otherwise           = Nothing
        se prev last [] = Just (last:prev)

opening = "<({["
closing = ">)}]"

close '<' = '>'
close '(' = ')'
close '[' = ']'
close '{' = '}'

score os = foldl helper 0 os
  where
    helper s c = s*5+score1 c

score1 '(' = 1
score1 '[' = 2
score1 '{' = 3
score1 '<' = 4


fromJust (Just n) = n
