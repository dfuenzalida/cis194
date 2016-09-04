{-# LANGUAGE FlexibleInstances, TypeSynonymInstances #-}

module Main where

import JoinList
import Sized
import Scrabble
import Buffer

import Editor

welcome = unlines
         [ "This buffer is for notes you don't want to save, and for"
         , "evaluation of steam valve coefficients."
         , "To load a different file, type the character L followed"
         , "by the name of the file."
         ]

welcomeJl = fromString welcome :: JoinList (Score, Size) String

main = runEditor editor welcomeJl
