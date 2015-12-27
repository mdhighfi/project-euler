import Data.List (sort)

main = print $ largestPalProd 100 999

largestPalProd :: Int -> Int -> Int
largestPalProd start stop =
  last . sort $ [
    i*j | i <- [start..stop],
          j <- [start..stop],
          i<=j,
          isPalindrome $ i*j
    ]

isPalindrome :: Int -> Bool
isPalindrome n =
  digits == reverse digits
  where digits = show n
