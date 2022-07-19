double x = x + x
quadruple x = (double . double) x

factorial x = product [1..x]
average xs = sum xs `div` length xs

rlast [x] = x
rlast (x:xs) = rlast xs

rinit [_] = []
rinit (x:xs) = x:(rinit xs)