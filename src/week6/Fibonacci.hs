{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

module Fibonacci where

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

-- https://stackoverflow.com/questions/6273621/understanding-a-recursively-defined-list-fibs-in-terms-of-zipwith
fibs2 :: [Integer]
fibs2 = 0 : 1 : zipWith (+) fibs2 (tail fibs2)

data Stream a =
  Cons a
       (Stream a)

streamToList :: Stream a -> [a]
streamToList (Cons s t) = s : streamToList t

instance Show a =>
         Show (Stream a) where
  show = show . take 20 . streamToList

streamRepeat :: a -> Stream a
streamRepeat n = Cons n (streamRepeat n)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons h t) = Cons (f h) $ streamMap f t

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f seed = Cons seed (streamFromSeed f (f seed))

nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons h s1) s2 = Cons h (interleaveStreams s2 s1)

ruler :: Stream Integer
ruler = loop 0

loop :: Integer -> Stream Integer
loop n = interleaveStreams (streamRepeat n) (loop (n + 1))

x :: Stream Integer
x = Cons 0 (Cons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger n = Cons n (streamRepeat 0)
  negate = streamMap negate
  (+) (Cons h1 t1) (Cons h2 t2) = Cons (h1 + h2) (t1 + t2)
  (*) (Cons h1 t1) b@(Cons h2 t2) = Cons (h1 * h2) (streamMap (* h1) t2 + t1 * b)

instance Fractional (Stream Integer) where
  (/) (Cons h1 t1) (Cons h2 t2) = q
    where
      q = Cons (h1 `div` h2) (streamMap (`div` h2) (t1 - (q * t2)))

fibs10 :: Stream Integer
fibs10 = x / (1 - x - (x * x))