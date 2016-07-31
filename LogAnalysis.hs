{-# OPTIONS_GHC -Wall #-}

module LogAnalysis where

import Log

----------------------------------------
-- Exercise 1
----------------------------------------

readInt :: String -> Int
readInt str = read str :: Int

nthWord :: String -> Int -> String
nthWord str n = words str !! n

dropWords :: String -> Int -> String
dropWords str n = unwords $ drop n $ words str

readNthInt :: String -> Int -> Int
readNthInt str n = readInt $ nthWord str n

parseMessage :: String -> LogMessage
parseMessage s = case (nthWord s 0) of
    "E" -> LogMessage (Error (readNthInt s 1)) (readNthInt s 2) (dropWords s 3)
    "I" -> LogMessage Info (readNthInt s 1) (dropWords s 2)
    "W" -> LogMessage Warning (readNthInt s 1) (dropWords s 2)
    _ -> Unknown s

parse :: String -> [LogMessage]
parse text = map parseMessage $ lines text

----------------------------------------
-- Exercise 2
----------------------------------------

insert :: LogMessage -> MessageTree -> MessageTree
insert lm Leaf = Node Leaf lm Leaf
insert lm@(LogMessage _ ts _) (Node l (LogMessage type0 ts0 msg0) r)
    | ts > ts0 = Node l (LogMessage type0 ts0 msg0) (insert lm r)
    | otherwise = Node (insert lm l) (LogMessage type0 ts0 msg0) r
insert _ mt = mt

----------------------------------------
-- Exercise 3
----------------------------------------

build :: [LogMessage] -> MessageTree
build = foldl (\m l -> insert l m) Leaf

