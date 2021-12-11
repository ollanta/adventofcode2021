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

neighbours (C x y) = [C x' y' | x' <- [x-1..x+1], y' <- [y-1..y+1], x'/=x || y'/=y]

solve inp = unlines [printM $ fst (states !! 0),
                     printM $ fst (states !! 1),
                     printM $ fst (states !! 2),
                     show (sum . map (M.size . snd) . take 101 $ states)]
  where
    chartelems = [(C x y, z) |
                (y, row) <- zip [0..] inp,
                (x, z)   <- zip [0..] row]
    chart = M.fromList chartelems

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

        tobump = concat . map neighbours . M.keys $ toflash'
        ch' = foldr (\k m -> M.adjust (+1) k m) ch tobump


printM :: M.HashMap Coord Integer -> String
printM ch = unlines [concat [show (ch M.! C x y) | x <- [0..maxx]] | y <- [0..maxy]]
  where
    maxx = maximum . map cx . M.keys $ ch
    maxy = maximum . map cy . M.keys $ ch

    cx (C x _) = x
    cy (C _ y) = y
