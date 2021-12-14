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

    -- every char is counted double, with an exception for the first and last one
    singleOccurences :: M.HashMap String Integer -> M.HashMap Char Integer
    singleOccurences occ = M.unionWith (+) dadjust . M.map (`div` 2) $ M.unionWith (-) chocc dadjust
      where
        chocc = M.fromListWith (+) . concatMap split $ M.toList occ
        split ((a:b:[]), v) = [(a,v), (b,v)]
        dadjust = M.fromListWith (+) [(head template, 1), (last template, 1)]

    getAnswer occ = maximum chocc - minimum chocc
      where
        chocc = singleOccurences occ
