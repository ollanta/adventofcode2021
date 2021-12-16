import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M
import Chart2d
import Parsing

main :: IO ()
main = optimisticInteract readD solve


readD :: Parser String
readD = many1 alphaNum


solve inp = unlines [show $ bin,
                     show $ readPackets bin,
                     show . sumversion . map fst $ readPackets bin,
                     show . map calc . map fst $ readPackets bin]
  where
    bin = fromHex inp

    sumversion (PNum v _ _:ps) = v + sumversion ps
    sumversion (POp v _ ips:ps) = v + sumversion ips + sumversion ps
    sumversion [] = 0

readPackets str
  | length str <= 6 = []
  | otherwise       = (packet, rest) : readPackets rest
  where
    (packet, rest) = readPacket str

version (PNum v _ _) = v
version (POp v _ _) = v

data Packet = PNum Integer Integer Integer | POp Integer Integer [Packet]
  deriving (Eq, Show)

readPacket :: String -> (Packet, String)
readPacket bin
  | typeid' == 4 = (PNum version typeid number, numbercont)
  | lengthtypeid == "0" = (POp version typeid pmop0, pmop0cont)
  | lengthtypeid == "1" = (POp version typeid pmop1, pmop1cont)
  where
    (versionbin, bin') = splitAt 3 bin
    (typeidbin, bin'') = splitAt 3 bin'

    version = fromBin versionbin
    typeid = fromBin typeidbin

    (number, numbercont) = readnumber "" bin''

    readnumber acc str
      | head str == '0' = (fromBin acc', rest)
      | head str == '1' = readnumber acc' rest
      where
        (next, rest) = splitAt 5 str
        acc' = acc ++ drop 1 next

    (lengthtypeid, bin''') = splitAt 1 bin''

    (pmop0, pmop0cont) = readpmop0 bin'''

    readpmop0 str = (packets, rest')
      where
        (lengthbin, rest) = splitAt 15 str
        lengthinbits = fromBin lengthbin
        (packetsbin, rest') = splitAt lengthinbits rest
        packets = map fst $ readPackets packetsbin

    (pmop1, pmop1cont) = readpmop1 bin'''
    readpmop1 str = (map fst packets, snd $ last packets)
      where
        (lengthbin, rest) = splitAt 11 str
        lengthinops = fromBin lengthbin

        packets = take lengthinops $ readPackets rest


calc (PNum v t n) = n
calc (POp v 0 ps) = sum $ map calc ps
calc (POp v 1 ps) = product $ map calc ps
calc (POp v 2 ps) = minimum $ map calc ps
calc (POp v 3 ps) = maximum $ map calc ps
calc (POp v 5 [p1, p2]) = if calc p1 > calc p2 then 1 else 0
calc (POp v 6 [p1, p2]) = if calc p1 < calc p2 then 1 else 0
calc (POp v 7 [p1, p2]) = if calc p1 == calc p2 then 1 else 0


fromBin str = fromBin' 0 str
  where
    fromBin' n (b:rest) = fromBin' (2*n+nb) rest
      where
        nb = if b == '1' then 1 else 0
    fromBin' n [] = n

fromHex = concatMap from1Hex

from1Hex '0' = "0000"
from1Hex '1' = "0001"
from1Hex '2' = "0010"
from1Hex '3' = "0011"
from1Hex '4' = "0100"
from1Hex '5' = "0101"
from1Hex '6' = "0110"
from1Hex '7' = "0111"
from1Hex '8' = "1000"
from1Hex '9' = "1001"
from1Hex 'A' = "1010"
from1Hex 'B' = "1011"
from1Hex 'C' = "1100"
from1Hex 'D' = "1101"
from1Hex 'E' = "1110"
from1Hex 'F' = "1111"
