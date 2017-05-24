module Golf where

import Data.List

-- | 1- Loop through the 1 to length of list `xs` and call `takeEveryNth`.
skips :: [a] -> [[a]]
skips xs = [takeEveryNth x xs | x <- [1 .. length xs]]

-- | 1- Create a list with items based on index to length of list `xs`.
-- 2- Filter the created list containing only items with multiplication of n.
-- 3- Loop through the filtered list and take desired element from list `xs` with !!(-1 is needed since !! starts from 0).
takeEveryNth :: Int -> [a] -> [a]
takeEveryNth index xs = [xs !! (idx - 1) | idx <- filter (\x -> x `mod` index == 0) [index .. length xs]]

-- | Pattern match with two conditions:
-- 1- First pattern: List should contain 3 items.
--    If maxima condition is met include that element and recursively call `localMaxima` again on list tail.
--    Otherwise just call `localMaxima` without including the element.
-- 2- Any other pattern should just return an empty list (If list contain no element or less than 3 items).
localMaxima :: [Integer] -> [Integer]
localMaxima (h:t@(x:y:_))
  | x > y && x > h = x : localMaxima t
  | otherwise = localMaxima t
localMaxima _ = []

histogram :: [Integer] -> String
histogram xs =
  let grouped = group (sort xs)
      maxLength = maximum (map length grouped)
  in unlines (map (line grouped) (reverse [1 .. maxLength])) ++ "==========\n0123456789\n"

line :: [[Integer]] -> Int -> String
line grouped n = stars (map head (filter (\xs -> length xs >= n) grouped))

stars :: [Integer] -> String
stars xs =
  concat
    [ if x `elem` xs
      then "*"
      else " "
    | x <- [0 .. 9]
    ]