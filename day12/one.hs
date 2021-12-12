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


solve inp = unlines [show edges,
                     show edgemap,
                     show . length $ genPaths]
  where
    edges = inp ++ map (\(a,b) -> (b,a)) inp
    edgemap = M.fromListWith (++) . map (\(k,v) -> (k,[v])) $ edges

    genPaths = gp [] "start"
      where
        gp :: [String] -> String -> [[String]]
        gp path st
          | st `elem` path && all isLower st = []
          | st == "end" = ["end":path]
          | otherwise   = concatMap (gp (st:path)) (edgemap M.! st)
