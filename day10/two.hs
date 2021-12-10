import Text.Parsec
import Data.List


main :: IO ()
main = do
  interact (solve . lines)
  putStrLn ""


solve inps = show $ answer
  where
    incomplete = filter (/=Nothing) . map parse $ inps
    scores = map (score . fromJust) $ incomplete
    answer = (!! (length scores `div` 2)) . sort $ scores

    parse brackets = helper [] brackets
      where
        helper p [] = Just p
        helper [] (next:rest)
          | opener next = helper [next] rest
          | otherwise   = Nothing
        helper (last:prev) (next:rest)
          | opener next        = helper (next:last:prev) rest
          | next == close last = helper prev rest
          | otherwise          = Nothing

opener c = c `elem` "<({["

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
