module Week4 where

import Data.List

fun1 :: [Integer] -> Integer
fun1 = product . map (subtract 2) . filter even

fun2 :: Integer -> Integer
fun2 =
  sum .
  filter even .
  takeWhile (> 1) .
  iterate
    (\x ->
       if even x
         then x `div` 2
         else 3 * x + 1)

data Tree a
  = Leaf
  | Node Integer
         (Tree a)
         a
         (Tree a)
  deriving (Show, Eq)

foldTree :: [a] -> Tree a
foldTree = foldr insertToTree Leaf

insertToTree :: a -> Tree a -> Tree a
insertToTree x Leaf = Node 0 Leaf x Leaf
insertToTree x (Node h tl value tr)
  | hr > hl = Node h newtl value tr
  | hr < hl = Node h tl value newtr
  | otherwise = Node (getTreeHeight newtl + 1) newtl value tr
  where
    hl = getTreeHeight tl
    hr = getTreeHeight tr
    newtl = insertToTree x tl
    newtr = insertToTree x tr

getTreeHeight :: Tree a -> Integer
getTreeHeight Leaf = 0
getTreeHeight (Node h _ _ _) = h

xor :: [Bool] -> Bool
xor =
  odd .
  length .
  foldl
    (\acc x ->
       if x
         then x : acc
         else acc)
    []

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> f x : acc) []

myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl f base xs = foldr (flip f) base (reverse xs)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n = map ((+ 1) . (* 2)) $ [1 .. n] \\ notNeeded
  where
    notNeeded = filter (<= n) . map (\(i, j) -> i + j + 2 * i * j) . cartProd [1 .. n] $ [1 .. n]

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x, y) | x <- xs, y <- ys]