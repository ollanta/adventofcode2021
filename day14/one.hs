import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M
import Parsing

main :: IO ()
main = optimisticInteract parser solve


parser :: Parser (String, [(String, String)])
parser = do
  template <- many1 alphaNum
  many1 newline
  pairs <- readPair `endBy` newline
  return (template, pairs)
  where
    readPair = do
      p1 <- many1 alphaNum
      string " -> "
      p2 <- many1 alphaNum
      return $ (p1, p2)


solve (template, pairs) = unlines [states !! 0,
                                   states !! 1,
                                   states !! 2,
                                   states !! 3,
                                   show . getAnswer $ states !! 10]
  where
    states = iterate nextstate template
    inserts = M.fromList pairs

    getAnswer st = last lst - head lst
      where
        lst = sort . map length . group . sort $ st

    nextstate :: String -> String
    nextstate (a:b:rest) = [a] ++ insert a b ++ nextstate (b:rest)
    nextstate l = l

    insert a b = M.lookupDefault "" (a:b:[]) inserts
