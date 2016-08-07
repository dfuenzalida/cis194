{-# OPTIONS_GHC -Wall #-}

----------------------------------------
-- Exercise 1
----------------------------------------

fun1 :: [Integer] -> Integer
fun1 []       = 1
fun1 (x:xs)
  | even x    = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun1' :: [Integer] -> Integer
fun1' xs = foldl (*) 1 $ map (pred . pred) $ filter even xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n | even n = n + fun2 (n `div` 2)
       | otherwise = fun2 (3 * n + 1)

----------------------------------------
-- Exercise 2
----------------------------------------

data Tree a = Leaf
            | Node Integer (Tree a) a (Tree a)
  deriving (Show, Eq)

height :: Tree a -> Integer
height Leaf = -1
height (Node _ l _ r) = succ $ max (height l) (height r)

folding :: (Eq a) => a -> Tree a -> Tree a
folding x Leaf = (Node 0 Leaf x Leaf)
folding x (Node h l y r)
  | height l < height r = (Node h (folding x l) y r)
  | otherwise = (Node h l y (folding x r))

foldTree :: (Eq a) => [a] -> Tree a
foldTree [] = Leaf
foldTree xs = foldr folding Leaf xs

----------------------------------------
-- Exercise 3
----------------------------------------

xor :: [Bool] -> Bool
xor xs = foldl (\a b -> if b then (not a) else a) False xs

map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x acc -> [(f x)] ++ acc) []

----------------------------------------
-- Exercise 4
----------------------------------------

cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

listDiff :: (Eq a) => [a] -> [a] -> [a]
listDiff [] _ = []
listDiff xs [] = xs
listDiff (x:xs) (y:ys)
  | x == y = listDiff xs ys
  | otherwise = [x] ++ listDiff xs (y:ys)

sieveSundaram :: Integer -> [Integer]
sieveSundaram n =
  let prod = cartProd [1..n] [1..n]
      pairs = filter (\(i,j) -> (i <= j)) prod
      bound = takeWhile (<=n) $ map (\(i,j)->i+j+(2*i*j)) pairs
  in map (succ . (2*)) $ listDiff [1..n] bound

