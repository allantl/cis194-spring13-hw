{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Calc where

import ExprT
import Parser
import StackVM

eval :: ExprT -> Integer
eval (ExprT.Lit n) = n
eval (ExprT.Add expr1 expr2) = eval expr1 + eval expr2
eval (ExprT.Mul expr1 expr2) = eval expr1 * eval expr2

evalStr :: String -> Maybe Integer
evalStr str = eval <$> parseExp ExprT.Lit ExprT.Add ExprT.Mul str

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = ExprT.Lit
  add = ExprT.Add
  mul = ExprT.Mul

reify :: ExprT -> ExprT
reify = id

instance Expr Integer where
  lit n = n
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)

instance Expr MinMax where
  lit = MinMax
  add (MinMax i1) (MinMax i2) = MinMax (max i1 i2)
  mul (MinMax i1) (MinMax i2) = MinMax (min i1 i2)

newtype Mod7 = Mod7 Integer deriving (Eq, Show)

instance Expr Mod7 where
  lit n = Mod7 (n `mod` 7)
  add (Mod7 i1) (Mod7 i2) = lit (i1 + i2)
  mul (Mod7 i1) (Mod7 i2) = lit (i1 * i2)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger = testExp :: Maybe Integer
testBool = testExp :: Maybe Bool
testMM = testExp :: Maybe MinMax
testSat = testExp :: Maybe Mod7

instance Expr StackVM.Program where
  lit i = [StackVM.PushI i]
  add p1 p2 = p1 ++ p2 ++ [StackVM.Add]
  mul p1 p2 = p1 ++ p2 ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile = parseExp lit add mul