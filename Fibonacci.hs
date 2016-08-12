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

