import Data.List (find)

main = print $ largestPalProd 3

largestPalProd :: Int -> Int
largestPalProd numDigits = multiplyOutward max max
  where max = 10^(numDigits+1) - 1

multiplyOutward :: Int -> Int -> Int
multiplyOutward start max = case firstPalProd of
  Nothing -> multiplyOutward (start-1) max
  Just a -> a
  where firstPalProd = find isPalindrome products
        products = if even start
          then zipWith (*) [start, start-1..] [start+1..max]
          else zipWith (*) [start, start-1..] [start..max]

isPalindrome :: Int -> Bool
isPalindrome n =
  digits == reverse digits
  where digits = show n
