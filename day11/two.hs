import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M
import Chart2d
import Parsing

main :: IO ()
main = optimisticInteract parser solve


parser :: Parser [[Integer]]
parser = readRow `endBy` newline
  where
    readRow = many1 readNum

    readNum = do
      n <- count 1 digit
      return $ read n


solve inp = unlines [showMS $ fst (states !! 0),
                     showMS $ fst (states !! 1),
                     showMS $ fst (states !! 2),
                     show . head $ dropWhile (\(sn, (ch, fl)) -> not . M.null $ M.difference ch fl) $ zip [0..] states]
  where
    chart = readM inp

    initstate = (chart, M.empty)

    states = iterate nextstate initstate

    nextstate :: (M.HashMap Coord Integer, M.HashMap Coord Integer) -> (M.HashMap Coord Integer, M.HashMap Coord Integer)
    nextstate (ch, _) = (ch''', flashed)
      where
        ch' = M.map (+1) ch
        (ch'', flashed) = runflashes ch' M.empty
        ch''' = M.union flashed ch''

    runflashes :: M.HashMap Coord Integer -> M.HashMap Coord Integer -> (M.HashMap Coord Integer, M.HashMap Coord Integer)
    runflashes ch flashed
      | M.null toflash' = (ch, flashed)
      | otherwise       = runflashes ch' flashed'
      where
        toflash = M.filter (>9) $ ch
        toflash' = M.difference toflash flashed
        flashed' = M.map (const 0) toflash' `M.union` flashed

        tobump = concat . map neighbours8 . M.keys $ toflash'
        ch' = foldr (\k m -> M.adjust (+1) k m) ch tobump
