module BalancedTernary where

import Data.List

data BtDigit = Minus | Zero | Plus deriving (Eq, Ord)

type BtNum = [BtDigit]

instance Enum BtDigit where
  fromEnum Minus = -1
  fromEnum Zero = 0
  fromEnum Plus = 1
  toEnum (-1) = Minus
  toEnum 0 = Zero
  toEnum 1 = Plus
  toEnum x = error ("toEnum on BtDigit doesn't handle anything outside (-1, 0, +1): received " ++ show x)

instance Show BtDigit where
  showsPrec _ Minus = ('-' :)
  showsPrec _ Zero = ('0' :)
  showsPrec _ Plus = ('+' :)
  showList = flip $ foldl $ flip shows

showAsList :: BtNum -> String
showAsList [] = ""
showAsList (x : xs) = show x ++ ":" ++ showAsList xs


instance Read BtDigit where
  readsPrec _ [] = []
  readsPrec _ (x : xs) = case x of
    '-' -> [(Minus, xs)]
    '0' -> [(Zero, xs)]
    '+' -> [(Plus, xs)]
    _ -> []
  readList xs = case reads xs of
    [] -> []
    (d, xs) : _ -> case readList xs of
      [] -> [([d], xs)]
      (ds, xs) : _ -> [(ds ++ [d], xs)] -- yes this is horrible, sorry

fromInteger' :: Integer -> BtNum
fromInteger' 0 = [Zero]
fromInteger' i = go i where 
  go 0 = []
  go i = toEnum (fromInteger (r - 1)) : (go q)
    where (q, r) = divMod (i + 1) 3 

toInteger' :: BtNum -> Integer
toInteger' = foldr f 0 where
  f d a = (3 * a) + fromIntegral (fromEnum d)

addDigit :: BtDigit -> BtDigit -> (BtDigit, BtDigit)
addDigit x Zero = (x, Zero)
addDigit Zero x = (x, Zero)
addDigit Plus Plus = (Minus, Plus)
addDigit Plus Minus = (Zero, Zero)
addDigit Minus Plus = (Zero, Zero)
addDigit Minus Minus = (Plus, Minus)

andCarry :: (BtDigit, BtDigit) -> BtDigit -> (BtDigit, BtDigit)
andCarry x Zero = x
andCarry (x, Zero) y = addDigit x y
andCarry (Minus, Plus) Plus = (Zero, Plus)
andCarry (Minus, Plus) Minus = (Plus, Zero)
andCarry (Plus, Minus) Plus = (Minus, Zero)
andCarry (Plus, Minus) Minus = (Zero, Minus)

add' :: BtNum -> BtNum -> BtNum
add' x y = add'' x y Zero where
  add'' [] [] Zero = []
  add'' [] [] x = [x]
  add'' xs ys c = r : add'' xs' ys' c' where
    safeSplit [] = (Zero, [])
    safeSplit (n:ns) = (n, ns)
    (x', xs') = safeSplit xs
    (y', ys') = safeSplit ys
    (r, c') = andCarry (addDigit x' y') c

negate' :: BtNum -> BtNum
negate' [] = []
negate' (Plus:xs) = Minus : negate' xs
negate' (Zero:xs) = Zero : negate' xs
negate' (Minus:xs) = Plus : negate' xs

mul :: BtNum -> BtNum -> BtNum
mul [] _ = []
mul (Plus : xs) ys = add' ys (mul xs (Zero:ys))
mul (Zero : xs) ys = mul xs (Zero:ys)
mul (Minus : xs) ys = add' (negate' ys) (mul xs (Zero:ys))

sign :: BtNum -> BtNum
sign xs = [sign' xs Zero] where
  sign' [] c = c
  sign' (Plus : xs) _ = sign' xs Plus
  sign' (Minus : xs) _ = sign' xs Minus
  sign' (Zero : xs) c = sign' xs c

abs' :: BtNum -> BtNum
abs' x = case sign x of
  [Minus] -> negate' x
  _ -> x

newtype BTNum = BTNum BtNum

instance Num BTNum where
  (BTNum x) + (BTNum y) = BTNum $ add' x y
  (BTNum x) * (BTNum y) = BTNum $ mul x y
  negate (BTNum x) = BTNum $ negate' x
  abs (BTNum x) = BTNum $ abs' x
  signum (BTNum x) = BTNum $ sign x
  fromInteger x = BTNum $ fromInteger' x
