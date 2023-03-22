--module Golf where

skips :: [a] -> [[a]]
skips input = [[a | (a, b) <- zip input [1 ..], mod b i == 0] | i <- [1 .. length input]]


main :: IO ()
main = do
  print (skips ["hello!"])
-- for each skip interval ( 1..len ) the input is zipped with 1 indexed range and filtered with mod

localMaxima :: [Integer] -> [Integer]
localMaxima (a : b : c : d) =
  if b > a && b > c
    then b : localMaxima (b : c : d)
    else localMaxima (b : c : d)
localMaxima _ = []

-- recursively compares first three elements prepended to rest, second case checks size < 3

histogram :: [Integer] -> String
histogram input = histList input ++ "==========\n0123456789\n"


histList :: [Integer] -> String
histList = concat . reverse . format . allCounts

format :: [Int] -> [String]
format input = map (\n -> [if x >= n then '*' else ' ' | x <- input]) [1..maximum input]


allCounts :: [Integer] -> [Int]
allCounts input = [length ( filter (==n) input) | n <- [0..9]]

-- not pretty, converts list to counter and formats lines from result

