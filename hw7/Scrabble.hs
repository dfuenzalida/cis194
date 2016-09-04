{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleInstances #-}
module Scrabble where

import Data.Monoid

newtype Score = Score Integer
  deriving (Eq, Ord, Show, Num)

instance Monoid Score where
  mempty = Score 0
  mappend = (+)

scoreTable :: [(Integer, String)]
scoreTable = [(1,"aeilnorstuAEILNORSTU"),(2,"dgDG"),(3,"bcmpBCMP"),
              (4,"fhvwyFHVWY"),(5,"kK"),(8,"jxJX"),(10,"qzQZ")]

score :: Char -> Score
score c = Score $ sum $ map (\(s,cs) -> if (any (==c) cs) then s else 0) scoreTable

scoreString :: String -> Score
scoreString s = foldl mappend mempty $ map score s

