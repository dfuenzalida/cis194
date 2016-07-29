{-# OPTIONS_GHC -Wall #-}

----------------------------------------
-- Exercise 1
----------------------------------------

toDigits    :: Integer -> [Integer]
toDigitsRev :: Integer -> [Integer]

toDigitsRev n
  | n < 1     = []
  | n < 10    = [n]
  | otherwise = (n `mod` 10) : (toDigitsRev (n `div` 10))

toDigits = reverse . toDigitsRev

----------------------------------------
-- Exercise 2
----------------------------------------

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther' :: [Integer] -> [Integer]

doubleEveryOther' [] = []
doubleEveryOther' (x:[]) = [x]
doubleEveryOther' (x:(y:zs)) = x : (2*y) : doubleEveryOther'(zs)

doubleEveryOther = reverse . doubleEveryOther' . reverse

----------------------------------------
-- Exercise 3
----------------------------------------

sumDigits :: [Integer] -> Integer
sumDigits xs = foldl (+) 0 (map (\x -> foldl (+) 0 (toDigits x)) xs)

----------------------------------------
-- Exercise 4
----------------------------------------

validate :: Integer -> Bool
validate n = ((sumDigits (doubleEveryOther (toDigits n))) `rem` 10) == 0

main = putStrLn (show (validate 4012888888881881))
