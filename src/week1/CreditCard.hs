module CreditCard where

toDigits :: Integer -> [Integer]
toDigits n
  | n > 0 = toDigits (n `div` 10) ++ [n `mod` 10]
  | otherwise = []

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs =
  let lstLength = length xs
  in reverse (fst (foldl (\(lst, index) x -> (doubleSecondIndex index x : lst, index - 1)) ([], lstLength) xs))

doubleSecondIndex :: Int -> Integer -> Integer
doubleSecondIndex index num =
  if index /= 0 && index `mod` 2 == 0
    then num * 2
    else num

sumDigits :: [Integer] -> Integer
sumDigits xs = sum (map (sum . toDigits) xs)

validate :: Integer -> Bool
validate x = (sumDigits . doubleEveryOther . toDigits $ x) `mod` 10 == 0
