-- 2
filterMap f p = (map f) . (filter p)

-- 3
map' f = foldr (\x xs -> f x : xs) []
filter' p = foldr (\x xs -> if p x then x:xs else xs) []