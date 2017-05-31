{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Risk where

import Control.Applicative
import Control.Monad.Random
import Data.List

------------------------------------------------------------
-- Die values
newtype DieValue = DV
  { unDV :: Int
  } deriving (Eq, Ord, Show, Num)

first :: (a -> b) -> (a, c) -> (b, c)
first f (a, c) = (f a, c)

instance Random DieValue where
  random = first DV . randomR (1, 6)
  randomR (low, hi) = first DV . randomR (max 1 (unDV low), min 6 (unDV hi))

die :: Rand StdGen DieValue
die = getRandom

------------------------------------------------------------
-- Risk
type Army = Int

data Battlefield = Battlefield
  { attackers :: Army
  , defenders :: Army
  } deriving (Show)

replicateDie :: Int -> Rand StdGen [DieValue]
replicateDie n = replicateM n die

battle :: Battlefield -> Rand StdGen Battlefield
battle Battlefield {attackers = att, defenders = def} = do
  attDies <- replicateDie (maxAtt att)
  defDies <- replicateDie (maxDef def)
  let Battlefield {attackers = attRes, defenders = defRes} = performBattle attDies defDies
  return Battlefield {attackers = attRes + (att - maxAtt att), defenders = defRes + (def - maxDef def)}

maxAtt :: Int -> Int
maxAtt att =
  if att < 4
    then max 0 (att - 1)
    else max 3 (att - 1)

maxDef :: Int -> Int
maxDef def =
  if def < 2
    then max 0 def
    else 2

performBattle :: [DieValue] -> [DieValue] -> Battlefield
performBattle attDies defDies =
  let sortedAtt = reverse . sort $ attDies
      sortedDef = reverse . sort $ defDies
      diff = zipWith (\x y -> (x - y, y - x)) sortedAtt sortedDef
      att = filter (> 0) (map fst diff)
      def = filter (>= 0) (map snd diff)
  in Battlefield
     {attackers = length att + (length attDies - length diff), defenders = length def + (length defDies - length diff)}

invade :: Battlefield -> Rand StdGen Battlefield
invade bfield = do
  result@Battlefield {attackers = attRes, defenders = defRes} <- battle bfield
  if (attRes < 2) || (defRes <= 0)
    then return result
    else invade result

successProb :: Battlefield -> Rand StdGen Double
successProb bf = replicateM 1000 (invade bf) >>= (\res -> return (fromIntegral (length (attList res)) / 1000))
  where
    attList = filter ((<= 0) . defenders)