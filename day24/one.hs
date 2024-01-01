{-# LANGUAGE FlexibleContexts #-}
import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M
import Chart2d
import Parsing
import qualified Data.Heap as H

main :: IO ()
main = optimisticInteract readD solve

data Var = V Integer | R Char
  deriving (Eq, Show)

data Op = Inp Var | Op String Var Var
  deriving (Eq, Show)

readD :: Parser [Op]
readD = readRow `endBy` newline
  where
    readRow = choice [readInp, readOp]

    readInp = do
      string "inp"
      spaces
      v <- readRVar
      return $ Inp v

    readOp = do
      op <- many1 letter
      spaces
      v1 <- readVar
      spaces
      v2 <- readVar
      return (Op op v1 v2)

    readVar = choice [readVVar, readRVar]

    readRVar = do
      s <- letter
      return (R s)

    readVVar = do
      n <- mnumber
      return (V n)
      

solve ops = unlines $ [
  show ops,
  "bac"
  ]
  where
    {-
    ustates = foldl' apply (M.singleton (0,0,0,0) []) ops
    highest = maximum . M.elems . M.filterWithKey (\(w,x,y,z) _ -> z == 0) $ ustates
    -}


data Calc = CInt Integer | CVar Char | COp String Calc Calc
  deriving (Show, Eq)

-- state -> maxinp
{-
apply inpmap (Inp (R c)) = M.fromList [(state', inp ++ [show i]) |
                                       i <- [1..9],
                                       (state, inp) <- M.toList inpmap,
                                       let state' = sinsert state c i]
apply inpmap (Op op (R c) v) = M.fromList [(state', inps) |
                                           (state, inps) <- M.toList inpmap,
                                           let v1 = sget state c,
                                           let v2 = getV state v,
                                           let state' = sinsert state c (apply' op v1 v2)]
  where
    getV state (R c) = sget state c
    getV state (V i) = i

    apply' "add" a b = a + b
    apply' "mul" a b = a * b
    apply' "div" a b = signum (a*b) * div (abs a) (abs b)
    apply' "mod" a b = mod a b
    apply' "eql" a b = if a == b then 1 else 0
    

sget (w,x,y,z) 'w' = w
sget (w,x,y,z) 'x' = x
sget (w,x,y,z) 'y' = y
sget (w,x,y,z) 'z' = z
  
sinsert (w,x,y,z) 'w' i = (i,x,y,z)
sinsert (w,x,y,z) 'x' i = (w,i,y,z)
sinsert (w,x,y,z) 'y' i = (w,x,i,z)
sinsert (w,x,y,z) 'z' i = (w,x,y,i)
-}

expandc c ops = (M.! c) . fst $ foldl' expand (initm, varinp) ops
  where
    initm = M.fromList . zip "wxyz" $ repeat (CInt 0)
    varinp = map CVar ['a'..'z']

showE (CInt v) = show v
showE (CVar v) = [v]
showE (COp "add" o1@(COp "mul" _ _) o2@(COp "mul" _ _)) = showE o1 ++ showOp "add" ++ showE o2
showE (COp "add" o1@(COp "add" _ _) o2@(COp "add" _ _)) = showE o1 ++ showOp "add" ++ showE o2
showE (COp "mul" o1@(COp "mul" _ _) o2@(COp "mul" _ _)) = showE o1 ++ showOp "mul" ++ showE o2
showE (COp "add" o1@(COp "add" _ _) o2@(CVar _)) = showE o1 ++ showOp "add" ++ showE o2
showE (COp "mul" o1@(COp "mul" _ _) o2@(CVar _)) = showE o1 ++ showOp "mul" ++ showE o2
showE (COp "add" o1@(COp "add" _ _) o2@(CInt _)) = showE o1 ++ showOp "add" ++ showE o2
showE (COp "mul" o1@(COp "mul" _ _) o2@(CInt _)) = showE o1 ++ showOp "mul" ++ showE o2
showE (COp op e1 e2) = showE' e1 ++ showOp op ++ showE' e2
  where
    showE' e@(COp _ _ _) = "(" ++ showE e ++ ")"
    showE' c = showE c

showOp "add" = "+"
showOp "mul" = "*"
showOp "div" = "/"
showOp "mod" = "%"
showOp "eql" = "=="


expand (m, (i:inps)) (Inp (R c)) = (M.insert c i m, inps)
expand (m, inps) (Op op (R c) v2) = (m', inps)
  where
    getC (R c) = m M.! c
    getC (V i) = CInt i

    m' = M.insert c v' m
    v' = exp' op (m M.! c) (getC v2)

exp' "add" i@(CInt _) v@(CVar _) = exp' "add" v i
exp' "add" i@(CInt _) o@(COp _ _ _) = exp' "add" o i
exp' "add" v@(CVar _) o@(COp _ _ _) = exp' "add" o v
exp' "mul" i@(CInt _) v@(CVar _) = exp' "mul" v i
exp' "mul" i@(CInt _) o@(COp _ _ _) = exp' "mul" o i
exp' "mul" v@(CVar _) o@(COp _ _ _) = exp' "mul" o v
exp' "eql" i@(CInt _) v@(CVar _) = exp' "eql" v i
exp' "eql" i@(CInt _) o@(COp _ _ _) = exp' "eql" o i
exp' "eql" v@(CVar _) o@(COp _ _ _) = exp' "eql" o v

exp' "add" a (CInt 0) = a
exp' "add" (CInt 0) a = a
exp' "add" (COp "add" e i@(CInt a)) (CInt b)   = exp' "add" e (CInt $ a+b)
exp' "add" (COp "add" e i@(CInt a)) v@(CVar _) = exp' "add" (exp' "add" e v) i
exp' "add" o (COp "add" e1 e2) = exp' "add" (exp' "add" o e1) e2

exp' "mul" _ (CInt 0) = CInt 0
exp' "mul" (CInt 0) _ = CInt 0
exp' "mul" v (CInt 1) = v
exp' "mul" (CInt 1) v = v
exp' "mul" (COp "mul" v (CInt a)) (CInt b) = exp' "mul" v (CInt $ a * b)
exp' "mul" (COp "mul" e i@(CInt a)) o = exp' "mul" (exp' "mul" e o) i
exp' "mul" o (COp "mul" e1 e2) = exp' "mul" (exp' "mul" o e1) e2
exp' "mul" (COp "add" a b) c = exp' "add" (exp' "mul" a c) (exp' "mul" b c)
exp' "mul" c (COp "add" a b) = exp' "add" (exp' "mul" a c) (exp' "mul" b c)

exp' "div" (CInt 0) _ = CInt 0
exp' "div" v (CInt 1) = v
exp' "div" (COp "mul" c (CInt a)) (CInt b)
  | mod a b == 0 = CInt 0
exp' "div" o@(COp "add" _ _) i@(CInt a)
  | o' == o   = COp "div" o i
  | otherwise = exp' "div" o' i
  where
    o' = divred o

    divred (CInt b)
      | mod b a == 0 = CInt 0
    divred (CVar v)
      | a > 9 = CVar v
    divred (COp "add" e1 e2) = exp' "add" (divred e1) (divred e2)
    divred (COp "mul" e1 (CInt b))
      | div b a == 0 = CInt 0
    divred e = e

exp' "mod" (CInt 0) _ = CInt 0
exp' "mod" e (CInt x)
  | x > cmax e = e
exp' "mod" (COp "mul" c (CInt a)) (CInt b)
  | mod a b == 0 = CInt 0
exp' "mod" o@(COp "add" _ _) i@(CInt a)
  | o' == o   = COp "mod" o i
  | otherwise = exp' "mod" o' i
  where
    o' = modred o

    modred (CInt b) = CInt (mod b a)
    modred (CVar v)
      | a > 9 = CVar v
    modred (COp "add" e1 e2) = exp' "add" (modred e1) (modred e2)
    modred (COp "mul" e1 (CInt b))
      | mod b a == 0 = CInt 0
    modred e = e
      
--exp' "mod" (COp "add" c1 c2) i = exp' "mod" (exp' "add" (exp' "mod" c1 i) (exp' "mod" c2 i)) i

exp' "eql" c1 c2
  | cmax c1 < cmin c2 = CInt 0
  | cmin c1 > cmax c2 = CInt 0

exp' "add" (CInt a) (CInt b) = CInt $ a + b
exp' "mul" (CInt a) (CInt b) = CInt $ a * b
exp' "div" (CInt a) (CInt b) = CInt $ signum (a*b) * div (abs a) (abs b)
exp' "mod" (CInt a) (CInt b) = CInt $ mod a b
exp' "eql" (CInt a) (CInt b) = CInt $ if a == b then 1 else 0

exp' op v1 v2 = COp op v1 v2

cmin c@(CVar _) = 1
cmin c@(CInt i) = i
cmin c@(COp op c1 c2)
  | op == "add" = cmin c1 + cmin c2
  | op == "mul" = minimum [v1*v2 | v1 <- [min1, max1], v2 <- [min2, max2]]
  | op == "div" && min2 > 0 && min1 > 0 = div min1 max2
  | op == "mod" = 0
  | op == "eql" = 0
  where
    min1 = cmin c1
    max1 = cmax c1
    min2 = cmin c2
    max2 = cmax c2

cmax c@(CVar _) = 9
cmax c@(CInt i) = i
cmax c@(COp op c1 c2)
  | op == "add" = cmax c1 + cmax c2
  | op == "mul" = maximum [v1*v2 | v1 <- [min1, max1], v2 <- [min2, max2]]
  | op == "div" && min2 > 0 && min1 > 0 = div max1 min2
  | op == "mod" = max2 - 1
  | op == "eql" = 1
  where
    min1 = cmin c1
    max1 = cmax c1
    min2 = cmin c2
    max2 = cmax c2


applyC op cinp = applyC' op
  where
    varm = (M.fromList $ zip ['a'..'z'] cinp)

    applyC' (CInt i) = i
    applyC' (CVar c) = varm M.! c
    applyC' (COp op c1 c2)
      | op == "add" = (+) v1 v2
      | op == "mul" = (*) v1 v2
      | op == "div" = (\a b -> signum (a*b) * div (abs a) (abs b)) v1 v2
      | op == "mod" = mod v1 v2
      | op == "eql" = if v1 == v2 then 1 else 0
      where
        v1 = applyC' c1
        v2 = applyC' c2
        


applyMany inps ops = foldl' apply (M.fromList . zip "wxyz" $ repeat 0, inps) ops

apply (m, (i:inps)) (Inp (R c)) = (M.insert c i m, inps)
apply (m, inps) (Op op v1 v2)
  | op == "add" = (upd (+) v1 v2, inps)
  | op == "mul" = (upd (*) v1 v2, inps)
  | op == "div" = (upd (\a b -> signum (a*b) * div (abs a) (abs b)) v1 v2, inps)
  | op == "mod" = (upd (mod) v1 v2, inps)
  | op == "eql" = (upd (\a b -> if a==b then 1 else 0) v1 v2, inps)
  where
    R c1 = v1

    val (V i) = i
    val (R c) = m M.! c

    upd f (R c) v2 = M.adjust (\v -> f v (val v2)) c m


{-          
genModelNumbers = [
  [i1, i2, i3, i4, i5, i6, i7, i8, i9, i10, i11, i12, i13, 14] |
   i1 <- all,
   i2 <- all,
   i3 <- [6],--all,
   i4 <- [9],--nub $ filter (<10) [9, i3+3],
   i5 <- all,
   i6 <- all,
   i7 <- [9],--all,
   i8 <- [1],--nub $ filter (>0) [9, i7-8],
   i9 <- all, -- ???
   i10 <- [7], --all,
   i11 <- [9], --nub $ filter (<10) [9, i10+2],
   i12 <- all,
   i13 <- [2],
   i14 <- [9]
   ]
  where
    all = [9,8,7,6,5,4,3,2,1]
-}

{-
inp x
mul x -1

w x y z

    inp a - Read an input value and write it to variable a.
    add a b - Add the value of a to the value of b, then store the result in variable a.
    mul a b - Multiply the value of a by the value of b, then store the result in variable a.
    div a b - Divide the value of a by the value of b, truncate the result to an integer, then store the result in variable a. (Here, "truncate" means to round the value toward zero.)
    mod a b - Divide the value of a by the value of b, then store the remainder in variable a. (This is also called the modulo operation.)
    eql a b - If the value of a and b are equal, then store the value 1 in variable a. Otherwise, store the value 0 in variable a.

c+3==d
g-8==h
j+2==k
i?

m+7=n
-}
