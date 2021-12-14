import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M
import Parsing
import Chart2d
import Data.Char

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


solve (template, pairs) = unlines [show template,
                                   show occurences,
                                   show $ states !! 1,
                                   show $ states !! 2,
                                   show $ states !! 10,
                                   show . getAnswer $ states !! 10,
                                   show . getAnswer $ states !! 40]
  where
    keys = zipWith (\a b -> a:b:[]) template (tail template)
    occurences = foldr (\k m -> M.insertWith (+) k 1 m) M.empty keys

    states = iterate nextstate occurences
    inserts = M.fromList pairs

    getAnswer :: M.HashMap String Integer -> Integer
    getAnswer st = (last lst - head lst)
      where
        st' = foldr (\(k,v) m -> M.insertWith (+) k v m) M.empty $ concatMap split (M.toList st)
        split ((a:b:[]), v) = [(a,v),(b,v)]
        st'' = M.mapWithKey dadjust st'

        dadjust k v
          | k `elem` [head template, last template] = ((v-1) `div` 2) + 1
          | otherwise = v `div` 2
        lst = sort . M.elems $ st''

    
    nextstate :: M.HashMap String Integer -> M.HashMap String Integer
    nextstate m = foldr (\(k,v) m -> M.insertWith (+) k v m) M.empty $ concatMap insert (M.toList m)
      where
        insert (k,v)
          | not $ k `M.member` inserts = [(k,v)]
          | otherwise                  = [(k1,v), (k2,v)]
          where
            i = inserts M.! k
            k1 = k !! 0 : i
            k2 = i ++ [k !! 1]


