import Text.Parsec
import Data.List
import Parsing


main :: IO ()
main = optimisticInteract parser solve

data Pairs = P Pairs Pairs | PN Integer
  deriving (Show, Eq)

parser :: Parser [Pairs]
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
    inp = ps
    total = foldl1 add (map reduce ps)

    largest = maximum [magnitude (add p1 p2) | p1 <- ps, p2 <- ps, p1 /= p2]

add p1 p2 = reduce $ P p1 p2

explode p = (b, p')
  where
    (b, p', _, _) = explode' 0 p

    explode' n (P (PN a) (PN b))
      | n == 4 = (True, PN 0, Just a, Just b)
    explode' n (P p1 p2)
      | exp1 = (True, P p1' p2p, addl1, Nothing)
      | exp2 = (True, P p1p p2', Nothing, addr2)
      | otherwise = (False, P p1 p2, Nothing, Nothing)
      where
        (exp1, p1', addl1, addr1) = explode' (n+1) p1
        (exp2, p2', addl2, addr2) = explode' (n+1) p2

        p2p = addleftmost addr1 p2
        p1p = addrightmost addl2 p1
    explode' n pn = (False, pn, Nothing, Nothing)

    addleftmost Nothing p = p
    addleftmost (Just a) (PN n) = PN (n+a)
    addleftmost a (P l r) = P (addleftmost a l) r

    addrightmost Nothing p = p
    addrightmost (Just a) (PN n) = PN (n+a)
    addrightmost a (P l r) = P l (addrightmost a r)
    
splitp p = (b, p')
  where
    (b, p') = splitp' p

    splitp' (PN n)
      | n >= 10 = (True, P (PN n2) (PN $ n-n2))
      | otherwise = (False, PN n)
      where
        n2 = n `div` 2
    splitp' (P p1 p2)
      | split1 = (True, P p1' p2)
      | split2 = (True, P p1 p2')
      | otherwise = (False, P p1 p2)
      where
        (split1, p1') = splitp' p1
        (split2, p2') = splitp' p2

reduce p
  | shouldexplode = reduce exploded
  | shouldsplit   = reduce splitted
  | otherwise     = p
  where
    (shouldexplode, exploded) = explode p
    (shouldsplit, splitted) = splitp p

magnitude (PN n) = n
magnitude (P l r) = 3 * magnitude l + 2 * magnitude r
