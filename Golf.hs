{-# OPTIONS_GHC -Wall #-}

module Golf where

----------------------------------------
-- Exercise 1
----------------------------------------

indexed :: [a] -> [(Integer, a)]
indexed xs = zip [1..] xs

every :: Integer -> [a] -> [a]
every n xs = map snd $ filter fst $ map (\(a,b) -> (a `mod` n == 0, b)) $ indexed xs

skips :: [a] -> [[a]]
skips xs = map (\n -> every (toInteger n) xs) [1..(length xs)]

----------------------------------------
-- Exercise 2
----------------------------------------

triplets ::  [a] -> [[a]]
triplets ns = map (\n -> take 3 (drop n ns)) [0..(length ns - 3)]

localMaxima :: [Integer] -> [Integer]
localMaxima ns = map (!! 1) $ filter (\[a,b,c] -> (a < b) && (b > c)) (triplets ns)

----------------------------------------
-- Exercise 3
----------------------------------------

groupDigs :: (Num a, Enum a) => [a] -> Int -> [a]
groupDigs ns n = concat[(take n ns), [(succ (ns !! n))], (drop (succ n) ns)]

genRow :: Integer -> [Integer] -> String
genRow n ns = map (\x -> if ((ns !! x) >= n) then '*' else ' ') [0..9]

histogram :: [Int] -> String
histogram xs =
  let grouped = foldl groupDigs [0,0,0,0,0,0,0,0,0,0] xs
      height = foldl max 0 grouped
      lins = map (\n -> genRow n grouped) [height, pred height..1]
      bottom = "==========\n0123456789"
  in (concat $ map (\s -> concat[s, "\n"]) lins) ++ bottom

-- test with: putStrLn $ histogram [1,4,5,4,6,6,3,4,2,4,9]
--
-- outputs:
--
--     *
--     *
--     * *
--  ******  *
-- ==========
-- 0123456789
