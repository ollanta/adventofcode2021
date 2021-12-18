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
    total = foldl1 add (map reduce ps)
    largest = maximum [magnitude (add p1 p2) | p1 <- ps, p2 <- ps, p1 /= p2]

add p1 p2 = reduce $ P p1 p2

explode p = (b, p')
  where
    (b, p', _, _) = explode' 0 p

    explode' n (P (PN a) (PN b))
      | n == 4 = (True, PN 0, Just a, Just b)
    explode' n (P l r)
      | n == 4 = error "This is not allowed"
      | lexp = (True, P l' lr, laddl, Nothing)
      | rexp = (True, P rl r', Nothing, raddr)
      | otherwise = (False, P l r, Nothing, Nothing)
      where
        (lexp, l', laddl, laddr) = explode' (n+1) l
        (rexp, r', raddl, raddr) = explode' (n+1) r

        lr = addleftmost laddr r
        rl = addrightmost raddl l
    explode' n pn = (False, pn, Nothing, Nothing)

    addleftmost Nothing p = p
    addleftmost (Just a) (PN n) = PN (n+a)
    addleftmost a (P l r) = P (addleftmost a l) r

    addrightmost Nothing p = p
    addrightmost (Just a) (PN n) = PN (n+a)
    addrightmost a (P l r) = P l (addrightmost a r)
    
splitp (PN n)
  | n >= 10   = (True, P (PN nl) (PN nr))
  | otherwise = (False, PN n)
  where
    nl = n `div` 2
    nr = n - nl
splitp (P l r)
  | splitl    = (True, P l' r)
  | splitr    = (True, P l r')
  | otherwise = (False, P l r)
  where
    (splitl, l') = splitp l
    (splitr, r') = splitp r

reduce p
  | shouldexplode = reduce exploded
  | shouldsplit   = reduce splitted
  | otherwise     = p
  where
    (shouldexplode, exploded) = explode p
    (shouldsplit, splitted) = splitp p

magnitude (PN n) = n
magnitude (P l r) = 3 * magnitude l + 2 * magnitude r
