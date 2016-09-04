{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer

data JoinList m a = Empty
                  | Single m a
                  | Append m (JoinList m a) (JoinList m a)
  deriving (Eq, Show)

------------------------------------------------------------
-- Exercise 1
------------------------------------------------------------
tag :: Monoid m => JoinList m a -> m
tag (Single m a) = m
tag (Append m _ _) = m
tag _ = mempty

(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
(+++) a b =
  let comb = mappend (tag a) (tag b)
  in Append comb a b

------------------------------------------------------------
-- Exercise 2
------------------------------------------------------------
-- Given
(!!?) :: [a] -> Int -> Maybe a
[]     !!? _         = Nothing
_      !!? i | i < 0 = Nothing
(x:xs) !!? 0         = Just x
(x:xs) !!? i         = xs !!? (i-1)

-- Given
jlToList :: JoinList m a -> [a]
jlToList Empty            = []
jlToList (Single _ a)     = [a]
jlToList (Append _ l1 l2) = jlToList l1 ++ jlToList l2

-- *JoinList> foldl (+++) Empty $ map (\x -> (Single (Size 1) x)) [1,2,3]
-- Append (Size 3) (Append (Size 2) (Append (Size 1) Empty (Single (Size 1) 1))
--   (Single (Size 1) 2)) (Single (Size 1) 3)


-- Ex 2.1
indexJ :: (Sized b, Monoid b) => Int -> JoinList b a -> Maybe a
indexJ _ Empty          = Nothing
indexJ 0 (Single m a)   = Just a
indexJ _ (Single m a)   = Nothing
indexJ i (Append m l r) =
  let sizeL = getSize $ size $ tag l
      sizeR = getSize $ size $ tag r
  in (if (i<sizeL) then indexJ i l else indexJ (i-sizeL) r)

-- *JoinList> indexJ 0 $ foldl (+++) Empty [Empty, Empty, Single (Size 1) 10, Empty]
-- Just 10
-- *JoinList> indexJ 1 $ foldl (+++) Empty [Empty, Empty, Single (Size 1) 10, Empty]
-- Nothing

-- Ex 2.2
dropJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
dropJ _ Empty          = Empty
dropJ 0 (Single m a)   = Single m a
dropJ _ (Single m a)   = Empty
dropJ i (Append m l r) =
  let sizeL = getSize $ size $ tag l
      sizeR = getSize $ size $ tag r
      res = if (i<sizeL) then (dropJ i l) +++ r else Empty +++ (dropJ (i-sizeL) r)
  in (if (i<0) then Empty else res)

-- Ex 2.3
takeJ :: (Sized b, Monoid b) => Int -> JoinList b a -> JoinList b a
takeJ 0 _              = Empty
takeJ _ Empty          = Empty
takeJ _ (Single m a)   = Single m a
takeJ i (Append m l r) =
  let sizeL = getSize $ size $ tag l
      sizeR = getSize $ size $ tag r
  in (if (i<sizeL) then (takeJ i l) +++ Empty else l +++ (takeJ (i-sizeL) r))

------------------------------------------------------------
-- Exercise 3
------------------------------------------------------------

-- These functions are implemented in Scrabble.hs:
-- score :: Char -> Score
-- scoreString :: String -> Score

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

------------------------------------------------------------
-- Exercise 4
------------------------------------------------------------

-- jlToString :: JoinList (Score, Size) String -> String
-- jlToString jl = unlines $ jlToList

stringsToJl :: [String] -> JoinList (Score, Size) String
stringsToJl [] = Empty
stringsToJl [s] = Single (scoreString s, Size 1) s
stringsToJl ss  =
  let n = (length ss) `div` 2
  in (stringsToJl $ take n ss) +++ (stringsToJl $ drop n ss)

instance Buffer (JoinList (Score, Size) String) where
  toString     = unlines . jlToList
  fromString   = stringsToJl . lines
  line n jl    =
    let firstLine = jlToList $ takeJ 1 $ dropJ n jl
    in (if (null firstLine) then Nothing else Just (head firstLine))
  replaceLine n l b =
    let bl = numLines b
        newBuf = (takeJ n b) +++ (stringsToJl [l]) +++ (dropJ (succ n) b)
    in (if ((n<0) || (n>(pred bl))) then b else newBuf)
  numLines     = getSize . size . tag
  value        = (\(Score s) -> fromInteger s :: Int) . fst . tag

-- *JoinList> fromString "hello\nworld" :: JoinList (Score, Size) String
-- Append (Score 17,Size 2) (Single (Score 8,Size 1) "hello") (Single (Score 9,Size 1) "world")
