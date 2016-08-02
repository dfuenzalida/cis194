{-# OPTIONS_GHC -Wall #-}

module Golf where

----------------------------------------
-- Exercise 1
----------------------------------------

indexed :: [a] -> [(Integer, a)]
indexed xs = zipWith (\a b -> (a,b)) [1..] xs

every :: Integer -> [a] -> [a]
every n xs = map snd $ filter fst $ map (\(a,b) -> (a `mod` n == 0, b)) $ indexed xs

skips :: [a] -> [[a]]
skips xs = map (\n -> every (toInteger n) xs) [1..(length xs)]
