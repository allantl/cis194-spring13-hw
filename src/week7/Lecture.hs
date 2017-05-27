{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lecture where

newtype Product a =
  Product a
  deriving (Eq, Ord, Num, Show)

getProduct :: Product a -> a
getProduct (Product a) = a

instance Num a =>
         Monoid (Product a) where
  mempty = Product 1
  mappend = (*)

instance Monoid Bool where
  mempty = False
  mappend = (&&)