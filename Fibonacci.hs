{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}

----------------------------------------
-- Exercise 1
----------------------------------------

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibs :: [Integer]
fibs = map fib [0..]

----------------------------------------
-- Exercise 2
----------------------------------------

fibs2 :: [Integer]
fibs2 = 0 : 1 : map (\n -> fibs2 !! (n-1) + fibs2 !! (n-2)) [2..]

----------------------------------------
-- Exercise 3
----------------------------------------

data Stream a = Stream a (Stream a)

kons :: a -> Stream a -> Stream a
kons a b = Stream a b

streamToList :: Stream a -> [a]
streamToList (Stream h r) = h : (streamToList r)

instance Show a => Show (Stream a) where
  show x = show $ take 20 $ streamToList x

----------------------------------------
-- Exercise 4
----------------------------------------

streamRepeat :: a -> Stream a
streamRepeat x = kons x (streamRepeat x)

streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Stream h r) = kons (f h) (streamMap f r)

streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = kons x (streamFromSeed f (f x))

----------------------------------------
-- Exercise 5
----------------------------------------

nats :: Stream Integer
nats = streamFromSeed succ 0

lgDiv :: Integer -> Integer
lgDiv x =
  let po2 = streamToList $ streamFromSeed (2*) 1
      smallPo2 = takeWhile (<=x) po2
      pairs = zip [0..] smallPo2
      fpairs = filter (\(a,b) -> x `mod` b == 0) pairs
  in last $ map fst fpairs

ruler :: Stream Integer
ruler = streamMap lgDiv $ streamMap succ nats

----------------------------------------
-- Exercise 6
----------------------------------------

x :: Stream Integer
x = kons 0 (kons 1 (streamRepeat 0))

instance Num (Stream Integer) where
  fromInteger :: Integer -> Stream Integer
  fromInteger a = kons a (streamFromSeed id 0)
  -- Use: show $ (fromInteger 12 :: Stream Integer)

  negate :: Stream Integer -> Stream Integer
  negate = streamMap ((-1)*)

  (+) :: Stream Integer -> Stream Integer -> Stream Integer
  (+) (Stream x a) (Stream y b) = kons (x+y) (a + b)

  (*) :: Stream Integer -> Stream Integer -> Stream Integer
  (*) (Stream x a) (Stream y b) =
    kons (x*y) ((streamMap (x*) b) + (a*(kons y b)))
  -- Use: (x+1)^2 returns: [1,2,1,0,0,0...]

instance Fractional (Stream Integer) where
  (/) :: Stream Integer -> Stream Integer -> Stream Integer
  (/) (Stream a0 a) (Stream b0 b) =
    let aqb = a + (negate (((kons a0 a) / (kons b0 b)) * b))
    in kons (a0 `div` b0) (streamMap (`div` b0) aqb)

fibs3 :: Stream Integer
fibs3 = x / (1 - x - x^2)
-- Returns: [0,1,1,2,3,5,8,13,21,34,55,89,144,233,377,610,987,1597,2584,4181...]

----------------------------------------
-- Exercise 7
----------------------------------------

data Matrix = Matrix Integer Integer Integer Integer
  deriving Show

instance Num Matrix where
  (*) :: Matrix -> Matrix -> Matrix
  (*) (Matrix a00 a01 a10 a11) (Matrix b00 b01 b10 b11) =
    Matrix (a00*b00+a01*b01) (a00*b10+a01*b11) (a10*b00+a11*b10) (a10*b01+a11*b11)

fib4 :: Integer -> Integer
fib4 x =
  let (Matrix a b c d) = (Matrix 1 1 1 0)^x
  in d

