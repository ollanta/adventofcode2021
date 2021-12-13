import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M
import Parsing
import Chart2d
import Data.Char

main :: IO ()
main = optimisticInteract parser solve


parser :: Parser ([Coord], [(Char, Integer)])
parser = do
  dots <- readPair `endBy` newline
  newline
  folds <- readFolds `endBy` newline
  return (dots, folds)
  where
    readPair = do
      n1 <- number
      string ","
      n2 <- number
      return $ (n1, n2)

    readFolds = do
      string "fold along "
      ax <- oneOf "xy"
      string "="
      v <- number
      return (ax, v)


solve (coords, folds) = unlines [showM lchart id,
                                 show . M.size $ chart']
  where
    chart = M.fromList $ zip coords (repeat "#")

    chart' = dofold chart (head folds)

    lchart = foldl dofold chart folds

    dofold :: M.HashMap Coord String -> (Char, Integer) -> M.HashMap Coord String
    dofold ch (ax, f) = M.union filtered folded
      where
        filtered = M.fromList [(c, v) | (c,v) <- M.toList ch, getax ax c < f]
        folded = M.fromList [(modax ax c, v) | (c,v) <- M.toList ch, getax ax c > f]

        modax :: Char -> Coord -> Coord
        modax 'x' (x,y) = (f - (x-f), y)
        modax 'y' (x,y) = (x, f - (y-f))

        getax 'x' (x,_) = x
        getax 'y' (_,y) = y
