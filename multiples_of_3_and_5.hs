multiples n = (rem n 3 == 0) || (rem n 5 == 0)
main = print $ sum $ takeWhile (<1000) $ filter multiples [1..]
