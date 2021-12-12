import Text.Parsec
import Data.List
import Data.Function
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
                     show . groupBy ((==) `on` fst) $ edges,
                     show paths,
                     show genPaths,
                     show . length $ genPaths]
  where
    edges = inp ++ map (\(a,b) -> (b,a)) inp
    paths = M.fromList . map (\l -> (fst $ head l, map snd l)) . groupBy ((==) `on` fst) . sort $ edges

    genPaths = gp [] "start"
      where
        goal = "end"

        gp :: [String] -> String -> [[String]]
        gp l st
          | st `elem` l && all isLower st = []
          | st == "end" = ["end":l]
          | otherwise   = concatMap (gp (st:l)) (paths M.! st)

