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

    genPaths = gp True [] "start"
      where
        goal = "end"

        gp :: Bool -> [String] -> String -> [[String]]
        gp haveTime l st
          | st == "start" && st `elem` l = []
          | st == "end" = ["end":l]
          | all isLower st && st `elem` l && not haveTime = []
          | all isLower st && st `elem` l = concatMap (gp False (st:l)) (paths M.! st)
          | otherwise   = concatMap (gp haveTime (st:l)) (paths M.! st)

