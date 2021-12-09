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
  deriving (Eq, Show, Generic)

instance Hashable Coord

neighbours (C x y) = [C (x+1) y, C (x-1) y, C x (y+1), C x (y-1)]

solve inp = unlines [show lowpoints, show (sum . map (+1) . M.elems $ lowpoints)]
  where
    chartelems = [(C x y, z) |
                (y, row) <- zip [0..] inp,
                (x, z)   <- zip [0..] row]
    chart = M.fromList chartelems

    getNeighbours c = map (chart M.!) . filter (`M.member` chart) $ neighbours c

    lowpoints = M.filterWithKey (\k v -> all ((>v)) (getNeighbours k)) chart
