{-# OPTIONS_GHC -Wall #-}

module Calc where

import ExprT
import Parser

----------------------------------------
-- Exercise 1
----------------------------------------

eval :: ExprT -> Integer
eval (Lit n) = n
eval (Mul a b) = (eval a) * (eval b)
eval (Add a b) = (eval a) + (eval b)
-- Test with: eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20

----------------------------------------
-- Exercise 2
----------------------------------------

evalMaybe :: Maybe ExprT -> Maybe Integer
evalMaybe Nothing = Nothing
evalMaybe (Just e) = Just $ eval e

evalStr :: String -> Maybe Integer
evalStr str =
  let parsed = parseExp Lit Add Mul str
  in evalMaybe parsed

----------------------------------------
-- Exercise 3
----------------------------------------

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit a = (Lit a)
  add a b = (Add a b)
  mul a b = (Mul a b)

-- Test with:
-- (mul (add (lit 2) (lit 3)) (lit 4) :: ExprT) == Mul (Add (Lit 2) (Lit 3)) (Lit 4)

-- also test:
reify :: ExprT -> ExprT
reify = id

-- Test with:
-- reify $ mul (add (lit 2) (lit 3)) (lit 4)
-- outputs: Mul (Add (Lit 2) (Lit 3)) (Lit 4)

----------------------------------------
-- Exercise 4
----------------------------------------

instance Expr Integer where
  lit a = a
  add a b = a + b
  mul a b = a * b

instance Expr Bool where
  lit a = (a > 0)
  add a b = a || b
  mul a b = a && b

newtype MinMax = MinMax Integer deriving (Eq, Show)
newtype Mod7   = Mod7 Integer deriving (Eq, Show)

instance Expr MinMax where
  lit a = MinMax a
  add (MinMax a) (MinMax b) = MinMax (max a b)
  mul (MinMax a) (MinMax b) = MinMax (min a b)

instance Expr Mod7 where
  lit a = Mod7 (a `mod` 7)
  add (Mod7 a) (Mod7 b) = Mod7 ((a + b) `mod` 7)
  mul (Mod7 a) (Mod7 b) = Mod7 ((a * b) `mod` 7)


