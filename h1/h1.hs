{-# OPTIONS_GHC -Wall #-}

toDigits :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]

toDigitsRev input
  | input <= 0 = []
  | otherwise = mod input 10 : toDigitsRev (div input 10)

toDigits input = reverse (toDigitsRev input)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther input = reverse (zipWith (*) (reverse input) (cycle [1,2]))

sumDigits :: [Integer] -> Integer
sumDigits input = sum [sum (toDigitsRev x) | x <- input]

validate :: Integer -> Bool

validate input = (sumDigits (doubleEveryOther (toDigits input)) `mod` 10) == 0

main :: IO ()

main = do
  print ( show (validate 4012888888881881) )
  print ( show (validate 4012888888881882) )
