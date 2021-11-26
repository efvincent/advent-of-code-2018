{-# OPTIONS_GHC -Wno-unused-imports   #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

-- |
-- Module      : AOC.Challenge.Day03
-- License     : BSD3
--
-- Stability   : experimental
-- Portability : non-portable
--
-- Day03a 
-- 

module AOC.Challenge.Day03 (
    day03a
  , day03b
  ) where

import AOC.Prelude ((:~>)(..))
import Data.Map as M (Map, fromList, insert, lookup, toList, empty)
import Data.Set as S (Set, fromList, member)
import Data.List.Split (splitOn, splitOneOf)

type Point = (Int,Int)
type PMap = Map Point Int
data Patch = Patch {
    pos :: Point
  , sz  :: Point
  , pnum:: Int
} deriving stock Show

-- | Parse one line into a patch
parseLine :: String -> Patch
parseLine l =
  Patch {
    pnum = read . drop 1 . head $ parts,
    pos = let pxy = map read . splitOn "," . (!!1) $ parts in (head pxy, pxy!!1),
    sz =  let shw = map read . splitOn "x" . (!!2) $ parts in (head shw, shw!!1)
  }
  where
    parts = splitOneOf "@:" l

-- | parses input into a list of patches. One patch per line
parse :: String -> [Patch]
parse =map parseLine . lines

-- | list of all points in a patch
points :: Patch -> [Point]
points p = 
  let (px,py) = pos p in
  let (w,h)   = sz  p in 
  [(x,y) | x <- [px..px+(w-1)], y <- [py..py+(h-1)]] 

-- | return the full frequency map for all the points in all the 
-- patches in the list of patches
pointFreqs :: [Patch] -> PMap
pointFreqs = 
  foldr (\patch pmap -> foldr incPointFreq pmap . points $ patch) M.empty 
  where
    incPointFreq :: Point -> PMap -> PMap
    incPointFreq p m = 
      case M.lookup p m of
        Just n  -> M.insert p (n+1) m
        Nothing -> M.insert p 1 m

solve1 :: [Patch] -> Int
solve1 = length . filter ((>1) . snd) . M.toList . pointFreqs 

solve2 :: [Patch] -> Maybe Int
solve2 patches = go patches
  where 
    overlappingPts = S.fromList . map fst . filter ((>1) . snd) . M.toList . pointFreqs $ patches

    -- | returns true if any of the points in the list is in the set. Short circuits
    -- first time it finds a point in the set
    anyInSet :: [Point] -> S.Set Point -> Bool
    anyInSet [] _ = False 
    anyInSet (p:ps) s = S.member p s || anyInSet ps s

    go :: [Patch] -> Maybe Int
    go [] = Nothing
    go (p:ps) = if anyInSet (points p) overlappingPts then go ps else Just . pnum $ p

day03a :: [Patch] :~> Int
day03a = MkSol
    { sParse = Just . parse
    , sShow  = show
    , sSolve = Just . solve1
    }

day03b :: [Patch] :~> Int
day03b = MkSol
    { sParse = Just . parse 
    , sShow  = show
    , sSolve = solve2
    }
