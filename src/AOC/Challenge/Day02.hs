{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day02
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day 2.  See "AOC.Solver" for the types used in this module!

module AOC.Challenge.Day02 (
     day02a
   , day02b
  ) where

import AOC.Prelude ( (:~>)(..), Map, isNothing )
import Data.Map as M (toList, empty, lookup, insert)

-- | Finds the frequencies of elements in a list
freqs :: Ord a => [a] -> Map a Int
freqs values =
  go values M.empty
  where
    go [] m = m
    go (x:xs) m = case M.lookup x m of
      Nothing -> go xs (M.insert x 1 m)
      Just freq -> go xs (M.insert x (freq + 1) m)

-- | Returns a list of target Frequencies (2 or 3 in this case)
-- | found in the list.
toFreqs :: Ord a => [a] -> [Int]
toFreqs = map fst . M.toList . freqs . filter (\n -> n == 2 || n == 3) . map snd . M.toList . freqs

-- | If the two elements differ by one element, return Just of the index
-- | of the differing element. Otherwise nothing
diffByOne :: Eq a => [a] -> [a] -> Maybe Int
diffByOne =
  go 0 Nothing
  where
    go _ b [] [] = b
    go i d (x1:xs1) (x2:xs2)
      | x1 == x2                = go (i+1) d        xs1 xs2
      | isNothing d && x1 /= x2 = go 0     (Just i) xs1 xs2
      | otherwise = Nothing
    go _ _ _ _ = Nothing

-- | Given a list of strings, compare each to every other, stopping when
-- | you find two that differ by exactly one character. Return a string (either
-- | of the two candidates) with the differing element removed
solve2b :: [String] -> String
solve2b s =
  go (s,s)
  where
    go = \case {
      (c1:cs1, c2:cs2) | c1 == c2 -> go (c1:cs1, cs2);
      (c1:cs1, c2:cs2) ->
        (case diffByOne c1 c2 of
          Just n -> take n c1 ++ drop (n + 1) c1
          Nothing -> go (c1:cs1, cs2));
      (_:c2:cs1, [])             -> go (c2:cs1, cs1);
      _                          -> error "no two string differ by exactly one character";
    }

day02a :: [String] :~> Int
day02a = MkSol
    { sParse = Just . lines
    , sShow  = show
    , sSolve = Just . product . map snd . M.toList . freqs . concatMap toFreqs
    }

day02b :: [String] :~> String
day02b = MkSol
    { sParse = Just . lines
    , sShow  = id
    , sSolve = Just . solve2b
    }
