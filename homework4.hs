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

folding :: (Eq a) => a -> Tree a -> Tree a
folding x Leaf = (Node 0 Leaf x Leaf)
folding x (Node h l y r)
  | l == Leaf = (Node h (folding x l) y r)
  | otherwise = (Node h l y (folding x r))

foldTree :: (Eq a) => [a] -> Tree a
foldTree [] = Leaf
foldTree xs = foldr folding Leaf xs

