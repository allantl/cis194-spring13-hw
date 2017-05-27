{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}

module Scrabble where

import Buffer
import Sized

newtype Score =
  Score Int
  deriving (Eq, Ord, Show, Num)

getScore :: Score -> Int
getScore (Score i) = i

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

score :: Char -> Score
score c
  | c' `elem` "aeilnorstu" = Score 1
  | c' `elem` "dg" = Score 2
  | c' `elem` "bcmp" = Score 3
  | c' `elem` "fhvwy" = Score 4
  | c' `elem` "k" = Score 5
  | c' `elem` "jx" = Score 8
  | c' `elem` "qz" = Score 10
  | otherwise = Score 0
  where
    c' = toLower c