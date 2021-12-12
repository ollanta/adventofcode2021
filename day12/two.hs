import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M
import Parsing
import Data.Char

main :: IO ()
main = optimisticInteract parser solve


parser :: Parser [(String, String)]
parser = readRow `endBy` newline
  where
    readRow = do
      s1 <- readStr
      string "-"
      s2 <- readStr
      return $ (s1, s2)

    readStr = many1 alphaNum


solve inp = unlines [show edgemap,
                     show . length $ genPaths]
  where
    edges = inp ++ map (\(a,b) -> (b,a)) inp
    edgemap = M.fromListWith (++) . map (\(k,v) -> (k,[v])) $ edges

    genPaths = gp True [] "start"
      where
        gp :: Bool -> [String] -> String -> [[String]]
        gp haveTime path st
          | st == "end"   = ["end":path]
          | st == "start" && isRevisit               = []
          | isRevisit && isSmallCave && not haveTime = []
          | isRevisit && isSmallCave && haveTime     = contPaths False
          | otherwise = contPaths haveTime
          where
            isRevisit = st `elem` path
            isSmallCave = all isLower st
  
            path' = st:path
            neighbours = edgemap M.! st
            contPaths ht = concatMap (gp ht path') neighbours
