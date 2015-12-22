main = print sumEvenFibs
  where sumEvenFibs = sum evenFibs
        evenFibs = filter even smallFibs
        smallFibs = takeWhile (<4000000) fibs
        fibs = 1 : 2 : (zipWith (+) fibs (tail fibs))
