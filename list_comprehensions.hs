pyths n = [(a, b, c) | a <- [1..n], b <- [1..n], c <- [1..a+b], a^2 + b^2 == c^2]

factors n = [x | x <- [1..n-1], n `mod` x == 0] -- Incorrect, should include n
perfect n = n == sum (factors n) -- When factors includes n, should init the list
perfects n = [x | x <- [1..n], perfect x]

scalar xs ys = sum [x*y | (x, y) <- zip xs ys]