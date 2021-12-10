import Text.Parsec
import Data.List


main :: IO ()
main = do
  interact (solve . lines)
  putStrLn ""


solve inps = show $ answer
  where
    errors = filter (/=Nothing) . map parse $ inps
    answer = sum . map (score . fromJust) $ errors

    parse brackets = helper [] brackets
      where
        helper _ [] = Nothing
        helper [] (next:rest)
          | opener next = helper [next] rest
          | otherwise   = Just next
        helper (last:prev) (next:rest)
          | opener next        = helper (next:last:prev) rest
          | next == close last = helper prev rest
          | otherwise          = Just next

opener c = c `elem` "<({["

close '<' = '>'
close '(' = ')'
close '[' = ']'
close '{' = '}'

score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137
score c   = error (c:[])

fromJust (Just n) = n
