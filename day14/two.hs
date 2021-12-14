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


solve (template, insertpairs) = unlines [show template,
                                   show $ states !! 0,
                                   show $ states !! 1,
                                   show $ states !! 2,
                                   show . singleOccurences $ states !! 10,
                                   show . singleOccurences $ states !! 40,
                                   show . getAnswer $ states !! 10,
                                   show . getAnswer $ states !! 40]
  where
    inserts = M.fromList insertpairs

    pairs = zipWith (\a b -> a:b:[]) template (tail template)
    occurences = M.fromListWith (+) $ zip pairs (repeat 1)

    states = iterate nextstate occurences

    nextstate :: M.HashMap String Integer -> M.HashMap String Integer
    nextstate m = M.fromListWith (+) . concatMap insert $ M.toList m
      where
        insert (k,v)
          | k `M.member` inserts = [(k1,v), (k2,v)]
          | otherwise            = [(k,v)]
          where
            i = inserts M.! k
            k1 = take 1 k ++ i
            k2 = i ++ drop 1 k

    singleOccurences :: M.HashMap String Integer -> M.HashMap Char Integer
    singleOccurences occ = M.fromListWith (+) $ (last template, 1):firstOccurences
      where
        firstOccurences = map split $ M.toList occ
        split (p, v) = (head p, v)

    getAnswer occ = maximum chocc - minimum chocc
      where
        chocc = singleOccurences occ
