module Hanoi where

type Peg = String

type Move = (Peg, Peg)

hanoi :: Integer -> String -> String -> String -> [Move]
hanoi discs p1 p2 p3
  | discs <= 0 = []
  | discs == 1 = [(p1, p2)]
  | otherwise = hanoi (discs - 1) p1 p3 p2 ++ hanoi 1 p1 p2 p3 ++ hanoi (discs - 1) p3 p2 p1