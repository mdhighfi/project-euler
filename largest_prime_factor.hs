import Data.List (find)

main = print $ largestPrimeFactor num
  where num = 600851475143

largestPrimeFactor :: Int -> Int
largestPrimeFactor n = case factor of
  Just a -> largestPrimeFactor $ div n a
  Nothing -> n
  where factor = find ((==0) . rem n) [2..(n-1)]
