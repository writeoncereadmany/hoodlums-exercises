{-# language PatternSynonyms, ViewPatterns, StandaloneDeriving, UndecidableInstances, DeriveFunctor, DeriveFoldable  #-}

module Interpreter where

import Data.Foldable

data ExprF r 
  = FApp r r
  | FAbs String r 
  | FVar String
  | FLitI Int
  | FAdd r r
  | FMul r r
  deriving (Show, Functor, Foldable)  

newtype Fix f = Fix {unfix ::  (f (Fix f)) }

deriving instance (Show (f (Fix f))) => Show (Fix f)
   
type Expr = Fix ExprF

pattern App a b = Fix (FApp a b)
pattern Abs i b = Fix (FAbs i b)
pattern Var s = Fix (FVar s)
pattern LitI x = Fix (FLitI x)
pattern Add a b = Fix (FAdd a b)
pattern Mul a b = Fix (FMul a b)

add1 :: Expr
add1 = Abs "n" (Add (Var "n") (LitI 1))

add1to4 :: Expr
add1to4 = App add1 (LitI 4)

type Env = [(String, Expr)]

eval :: Env -> Expr -> Expr
eval e (App (eval e -> Abs i a) (eval e -> b)) = eval ((i, b):e) a
eval e (Var (flip lookup e -> Just v)) = v
eval e (Add (eval e -> LitI x) (eval e -> LitI y)) = LitI (x + y)
eval e (Mul (eval e -> LitI x) (eval e -> LitI y)) = LitI (x * y)
eval _ a = a

cata :: (Functor f) => (f a -> a) -> Fix f -> a
cata alg = alg . fmap (cata alg) . unfix

data Value 
  = VLitI Int
  | VFunc (Value -> Value)

instance Show Value where
 show (VLitI a) = show a
 show (VFunc _) = "function"

type VEnv = [(String, Value)]

evalAlg :: ExprF (VEnv -> Value) -> VEnv -> Value
evalAlg (FApp fa b) e | VFunc a <- fa e = a (b e)
evalAlg (FAbs i b) e = VFunc (\v -> b ((i, v) : e))
evalAlg (FVar i) e | Just v <- lookup i e = v
evalAlg (FLitI v) _ = VLitI v
evalAlg (FAdd fa fb) e | VLitI a <- fa e, VLitI b <- fb e = VLitI (a + b)
evalAlg (FMul fa fb) e | VLitI a <- fa e, VLitI b <- fb e = VLitI (a * b)

litAlg :: ExprF [Int] -> [Int]
litAlg (FLitI v) = [v]
litAlg x = fold x

exLit :: Expr -> [Int]
exLit x = cata litAlg x

eval' :: Expr -> Value
eval' x  = cata evalAlg x []
