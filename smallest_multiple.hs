main = print $ smallestPos 20


smallestPos :: Int -> Int
smallestPos n = foldl (*) 1 $ smallestPosFactors n

smallestPosFactors :: Int -> [Int]
smallestPosFactors 1 = [1]
smallestPosFactors n = (newFactor n prev) : prev
  where prev = smallestPosFactors (n-1)


newFactor :: Int -> [Int] -> Int
newFactor n oldFactors =
  foldl divIfCan n oldFactors

divIfCan :: Int -> Int -> Int
divIfCan n m =
  if rem n m == 0
  then div n m
  else n
