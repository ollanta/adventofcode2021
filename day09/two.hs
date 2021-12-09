{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics (Generic)
import Data.Hashable
import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M


main :: IO ()
main = do
  interact (solve . readD)
  putStrLn ""


readD :: String -> [[Integer]]
--readD :: String -> Either ParseError [[Integer]]
readD s = inp
  where
    rinp = parse (readRow `endBy` newline) "" s
    Right inp = rinp

    readRow = many1 readNum

    readNum = do
      n <- count 1 digit
      return $ read n


data Coord = C Integer Integer
  deriving (Eq, Show, Generic, Ord)

instance Hashable Coord

neighbours (C x y) = [C (x+1) y, C (x-1) y, C x (y+1), C x (y-1)]

solve inp = unlines [show chart,
                     show basinMap,
                     show basinSizes,
                     show answer]
  where
    chartelems = [(C x y, z) |
                (y, row) <- zip [0..] inp,
                (x, z)   <- zip [0..] row]
    chart = M.fromList chartelems

    getNeighbours c = filter (`M.member` chart) $ neighbours c

    genBasin c@(C x y) bm
      | c `M.member` bm     = bm
      | z == 9              = M.insert c Nothing bm
      | null lowerpoints    = M.insert c (Just c) bm
      | length basins == 1  = M.insert c (head basins) bm'
      | otherwise           = M.insert c Nothing bm'
      where
        z = chart M.! c
        lowerpoints = filter ((<z) . (chart M.!)) (getNeighbours c)

        bm' = foldr genBasin bm lowerpoints
        basins = nub $ map (bm' M.!) lowerpoints

    basinMap = foldr genBasin M.empty (M.keys chart)

    basinSizes = map length . group . sort . filter (/=Nothing) $ M.elems basinMap
    answer = product . take 3 . reverse . sort $ basinSizes
