{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}
{-# LANGUAGE LambdaCase #-}
-- |
-- Module      : AOC.Challenge.Day01
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 1.  See "AOC.Solver" for the types used in this module!
--

module AOC.Challenge.Day01 (
    day01a
  , day01b
  ) where

import AOC.Solver (  (:~>)(..) )
import Data.IntSet (fromList, member, insert, singleton)

-- | @read@ function doesn't parse the plus in "+10". Remove the
-- leading plus if it's the first character in the string
unplus :: String -> Int
unplus = read . \case {
    '+':rest -> rest;
    x -> x
  }

-- | use @cycle@ to create an infinite list that loops from the input list,
-- accumulating the result of adding the next number in the list in an 
-- @IntSet@ until we see one that has been seen before.
solve1b :: [Int] -> Int
solve1b nums =
  go (singleton 0) 0 (cycle nums)
  where
    go seen cur (x:xs) 
      | member (x+cur) seen = x+cur
      | otherwise = go (insert (x+cur) seen) (x+cur) xs
    go _ _ [] = error "We'll never see this; cycle produces infinite list"

day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = Just . map unplus . lines
    , sShow  = show
    , sSolve = Just . sum
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = Just . map unplus . lines
    , sShow  = show
    , sSolve = Just . solve1b 
    }
