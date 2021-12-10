import Text.Parsec
import Data.List


main :: IO ()
main = do
  interact (solve . lines)
  putStrLn ""


solve inps = show . sum $ answer
  where
    answer = map (score . fromJust) . filter (/=Nothing) . map syntaxerror $ inps

    syntaxerror (c:cs) = se [] c cs
      where
        se prev last (next:rest)
          | next `elem` opening = se (last:prev) next rest
          | next == close last  = se (drop 1 prev) (head prev) rest
          | otherwise           = Just next
        se _ _ [] = Nothing


opening = "<({["
closing = ">)}]"

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
