-- (1)
---- (a)
safetaila xs = if null xs then [] else tail xs

---- (b)
safetailb xs | null xs = []
             | otherwise = tail xs

---- (c)
safetailc [] = []
safetailc (x:xs) = xs

-- (2)
ppa True  _ = True
ppa False b = b

ppb False False = False
ppb _     _     = True

ppc True  True  = True
ppc True  False = True
ppc False True  = True
ppc False False = False

-- (3)
(aaa) a b = if a 
                then if b 
                    then True 
                    else False
                else False


-- (4)
(aab) a b = if a
                then b
                else False