import Text.Parsec
import Data.List
import Parsing


main :: IO ()
main = optimisticInteract parser solve

data Pairs a = P (Pairs a) (Pairs a) | PN a
  deriving (Show, Eq)

parser :: Parser [Pairs Integer]
parser = readPairs `sepEndBy` newline
  where
    readPairs = choice [readP, readPN]

    readP = do
      string "["
      p1 <- readPairs
      string ","
      p2 <- readPairs
      string "]"
      return $ P p1 p2

    readPN = do
      n <- many1 digit
      return $ PN (read n)

solve ps = unlines $
  [show total,
   show (magnitude total),
   show $ largest
  ]
  where
    total = foldl1 add (map reduce ps)

    largest = maximum [magnitude (add p1 p2) | p1 <- ps, p2 <- ps, p1 /= p2]

add p1 p2 = reduce $ P p1 p2

traversep travf initst pairs = (st, p)
  where
    (p, st, _) = transform 0 initst (id,id,id) pairs

    transform lev st (mp, mc, mn) p = (p'', st'', (tmp . mp', id, tmn . mn'))
      where
        (p', st', (tmp, tmc, tmn)) = travf lev st p
        (p'', st'', (mp', mc', mn')) = traverse' lev st' (mp, tmc . mc, mn) p'

    traverse' lev st (mp,mc,mn) (PN n)  = (PN (mc n), st, (mp,id,mn))
    traverse' lev st (mp,mc,mn) (P l r) = (P l' r', st'', (mpl,id,mnr))
      where
        (l', st',  (mpl,_,mnl)) = transform (lev+1) st  (merge l (mp, mc, mpr)) l
        (r', st'', (mpr,_,mnr)) = transform (lev+1) st' (merge r (mnl, mc, mn)) r

        merge (PN _) (p,c,n) = (id, p . c . n, id)
        merge _      mods    = mods

explode :: Pairs Integer -> (Bool, Pairs Integer)
explode p = traversep exptr False p
  where
    exptr n False (P (PN a) (PN b))
      | n == 4 = (PN 0, True, ((+a), id, (+b)))
    exptr n st p = (p, st, (id, id, id))

splitp :: Pairs Integer -> (Bool, Pairs Integer)
splitp p = traversep splittr False p
  where
    splittr _ False (PN n)
      | n >= 10 = (P (PN nl) (PN nr), True, (id, id, id))
      where
        nl = n `div` 2
        nr = n - nl
    splittr _ st p = (p, st, (id, id, id))

reduce p
  | shouldexplode = reduce exploded
  | shouldsplit   = reduce splitted
  | otherwise     = p
  where
    (shouldexplode, exploded) = explode p
    (shouldsplit, splitted) = splitp p

magnitude (PN n) = n
magnitude (P l r) = 3 * magnitude l + 2 * magnitude r
