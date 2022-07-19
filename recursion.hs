iainit acc [_] = acc
iainit acc (x:xs) = iainit (acc ++ [x]) xs
ainit = iainit []

-- (1)
and' []         = True
and' (False:xs) = False
and' (True:xs)  = and' xs

concat' [] = []
concat' (xs:xss) = xs ++ concat' xss

replicate' 0 _ = []
replicate' n e = e:(replicate' (n-1) e)

index (x:_)  0 = x
index (_:xs) n = xs `index` (n-1)

elem' _ []     = False
elem' e (x:xs) | e == x    = True
               | otherwise = elem' e xs 

-- (2)
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x:(merge xs (y:ys))
                    | y < x  = y:(merge (x:xs) ys)

-- (3)
msort xs | length xs <= 1 = xs
         | otherwise = merge (msort (take m xs)) (msort (drop m xs))
            where
                m = length xs `div` 2