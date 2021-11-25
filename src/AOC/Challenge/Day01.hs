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
-- After completing the challenge, it is recommended to:
--
-- *   Replace "AOC.Prelude" imports to specific modules (with explicit
--     imports) for readability.
-- *   Remove the @-Wno-unused-imports@ and @-Wno-unused-top-binds@
--     pragmas.
-- *   Replace the partial type signatures underscores in the solution
--     types @_ :~> _@ with the actual types of inputs and outputs of the
--     solution.  You can delete the type signatures completely and GHC
--     will recommend what should go in place of the underscores.

module AOC.Challenge.Day01 (
    day01a
  , day01b
  ) where

import AOC.Solver
import Data.IntSet (fromList, member, insert, singleton)

dep :: String -> Int
dep = read . \case {
    '+':rest -> rest;
    x -> x
  }

p2 :: [Int] -> Int
p2 nums =
  go (singleton 0) 0 (cycle nums)
  where
    go seen cur xs =
      if member y seen 
        then y 
        else go (insert y seen) y rest
      where
        y = head xs + cur
        rest = tail xs


day01a :: [Int] :~> Int
day01a = MkSol
    { sParse = Just . map dep . lines
    , sShow  = show
    , sSolve = Just . sum
    }

day01b :: [Int] :~> Int
day01b = MkSol
    { sParse = Just . map dep . lines
    , sShow  = show
    , sSolve = Just . p2 
    }
