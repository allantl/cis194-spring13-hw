{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module JoinList where

import Buffer
import Data.Monoid
import Scrabble
import Sized
import Editor

data JoinList m a
  = Empty
  | Single m
           a
  | Append m
           (JoinList m a)
           (JoinList m a)
  deriving (Eq, Show)

(+++)
  :: Monoid m
  => JoinList m a -> JoinList m a -> JoinList m a
(+++) j1 j2 = Append (tag j1 `mappend` tag j2) j1 j2

tag
  :: Monoid m
  => JoinList m a -> m
tag (Single m _) = m
tag (Append m _ _) = m
tag _ = mempty

(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i
  | i < 0 = Nothing
(x:_) !!? 0 = Just x
(_:xs) !!? i = xs !!? (i - 1)

jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

jlSize :: (Sized b, Monoid b) => JoinList b a -> Int
jlSize = getSize . size . tag

indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ 0 (Single _ a) = Just a
indexJ n jl@(Append s l r)
  | n < 0 || (n + 1) > jlSize jl = Nothing
  | n >= jlSize l = indexJ (n - jlSize l) r
  | otherwise = indexJ n l
index _ _ = Nothing

dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ 1 (Single _ _) = Empty
dropJ n jl@(Append s l r)
  | n < 0 = jl
  | n > jlSize jl = jl
  | n == jlSize l = r
  | n < jlSize l = dropJ n l +++ r
  | n > jlSize l = dropJ (n - jlSize l) r
dropJ _ jl = jl

takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 1 s@(Single _ _) = s
takeJ n jl@(Append s l r)
  | n < 0 = Empty
  | n >= jlSize jl = jl
  | n == jlSize l = l
  | n < jlSize l = takeJ n l
  | n > jlSize l = l +++ takeJ (n - jlSize l) r
takeJ _ jl = Empty

scoreFromString :: String -> Score
scoreFromString = mconcat . map score

scoreLine :: String -> JoinList Score String
scoreLine str = Single (scoreFromString str) str

instance Buffer (JoinList (Score, Size) String) where
  toString = unlines . jlToList
  fromString str = foldl (\acc line -> acc +++ singleFromString line) Empty (lines str)
  line = indexJ
  replaceLine index newString buffer =
    case indexJ index buffer of
      Nothing -> buffer
      _ -> takeJ index buffer +++ singleFromString newString +++ dropJ (index + 1) buffer
  numLines = getSize . snd . tag
  value = getScore . fst . tag

singleFromString :: String -> JoinList (Score, Size) String
singleFromString str = Single (scoreFromString str, Size 1) str

main =
  runEditor
    editor
    (fromString $
     unlines
       [ "This buffer is for notes you don't want to save, and for"
       , "evaluation of steam valve coefficients."
       , "To load a different file, type the character L followed"
       , "by the name of the file."
       ] :: (JoinList (Score, Size) String))