{-# LANGUAGE FlexibleContexts #-}
import Text.Parsec
import Data.List
import qualified Data.HashMap.Strict as M
import qualified Data.HashSet as S
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
  --showE $ zinit,
  showE . snd $ relations !! 2,
  showE . snd $ relations !! 3,
  unlines $ [v : " == " ++ showE e | (v, e) <- M.toList finalRelations],
  show $ (maximum goodModelNumbers, minimum goodModelNumbers)
  ]
  where
    relations = recurseRelations initVarmap
    varNames = take 14 ['a'..'z']
    initVarmap = M.fromList [(c, CVar c) | c <- varNames]

    goodModelNumbers = genModelNumbers finalRelations
    genModelNumbers rels = helper M.empty varNames
      where
        -- Assuming there's only one relation per variable, generate symmetric relations
        toSimple (CVar a) = (a,0)
        toSimple (COp "add" (CVar a) (CInt k)) = (a,k)
        simpleRels = M.map toSimple rels
        simpleRevRels = M.fromList [(a,(b,-k)) | (b,(a,k)) <- M.toList simpleRels]
        combinedRels = M.unions [M.filter ((/=0).snd) simpleRels, M.filter ((/=0).snd) simpleRevRels, simpleRels]

        helper :: M.HashMap Char Integer -> [Char] -> [String]
        helper vars [] = [foldr1 (++) [show (vars M.! c) | c <- varNames]]
        helper vars (a:vs) = [mn | i <- vrange, mn <- helper (M.insert a i vars) vs]
          where
            (b, k) = combinedRels M.! a
            minA = max 1 (1+k)
            maxA = min 9 (9+k)

            vrange = case M.member b vars of
              True -> [vars M.! b + k]
              False -> [minA..maxA]


    {-
    Optimistic solution hoping the resulting expression will have simple relations between
    the inputs, like "c+3==d", and that they need to be true for the total calculation to
    be zero.
    -}
    finalRelations = fst (last relations)
    recurseRelations varmap
      | M.null newRelations = [(varmap, zexpression)]
      | otherwise           = (varmap, zexpression) : recurseRelations varmap'
      where
        initRegs = M.fromList . zip "wxyz" $ repeat (CInt 0)
        inputs = map (varmap M.!) varNames
        zexpression = (M.! 'z') . fst $ foldl' expand (initRegs, inputs) ops

        newRelations = findRelations zexpression

        findRelations (COp "eql" e@(COp "add" (CVar a) (CInt k)) (CVar b)) = M.singleton b e
        findRelations (COp _ o1 o2) = M.union (findRelations o1) (findRelations o2)
        findRelations _             = M.empty

        varmap' = M.union newRelations varmap


data Calc = CInt Integer | CVar Char | COp String Calc Calc
  deriving (Show, Eq)

expandc c ops  = (M.! c) . fst $ foldl' expand (initm, varinp) ops
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
exp' "div" v (CInt x)
  | x > cmax v && 0 <= cmin v = CInt 0
exp' "div" (COp "mul" c (CInt a)) (CInt b)
  | mod a b == 0 = CInt 0
exp' "div" (COp "div" c (CInt a)) (CInt b) = exp' "div" c (CInt (a*b))
exp' "div" o@(COp "add" _ _) i@(CInt a)
  | od == CInt 0 = COp "div" o i
  | otherwise    = exp' "add" (exp' "div" or i) od
  where
    (od, or) = divred o

    divred (CInt b) = (CInt (b `div` a), CInt (b `mod` a))
    divred (COp "add" e1 e2) = (exp' "add" e1d e2d, exp' "add" e1r e2r)
      where
        (e1d, e1r) = divred e1
        (e2d, e2r) = divred e2
    divred (COp "mul" e1 (CInt b))
      | mod b a == 0 = (exp' "mul" e1 (CInt (b `div` a)), CInt 0)
    divred e = (CInt 0, e)

exp' "mod" (CInt 0) _ = CInt 0
exp' "mod" e (CInt x)
  | x > cmax e = e
exp' "mod" (COp "mul" c (CInt a)) (CInt b)
  | mod a b == 0 = CInt 0
exp' "mod" (COp "div" c (CInt a)) (CInt b) = exp' "div" (exp' "mod" c (CInt (a*b))) (CInt a) -- <- NEW
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
  | c1 == c2 = CInt 1
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
  | op == "div" && min2 > 0 && min1 >= 0 = div min1 max2
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
  | op == "div" && min2 > 0 && min1 >= 0 = div max1 min2
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
