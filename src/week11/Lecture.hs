module Lecture where

import Control.Applicative hiding ((*>))

(*>)
  :: Applicative f
  => f a -> f b -> f b
(*>) = liftA2 (\_ y -> y)

seqA :: (Applicative f) => [f a] -> f [a]
seqA [] = pure []
seqA (x : xs) = (:) <$> x <*> (seqA xs)

mapA
  :: Applicative f
  => (a -> f b) -> [a] -> f [b]
mapA f = foldr (liftA2 (:) . f) (pure [])

repA :: Applicative f => Int -> f a -> f [a]
repA n = fmap (replicate n)