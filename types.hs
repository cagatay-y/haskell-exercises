-- 1
data Nat = Zero | Succ Nat

add :: Nat -> Nat -> Nat
add Zero n = n
add (Succ m) n = Succ (m `add` n)

multiply :: Nat -> Nat -> Nat
multiply Zero n = Zero
multiply (Succ m) n = n `add` (m `multiply` n)

-- 3
data Tree a = Leaf a | Node (Tree a) (Tree a)
