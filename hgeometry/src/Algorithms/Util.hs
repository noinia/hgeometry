module Algorithms.Util where

import qualified Data.List as L


data SP a b = SP !a !b deriving (Eq,Ord,Show)

-- | Given a list xs, generate all unique (unordered) pairs.
uniquePairs    :: [a] -> [SP a a]
uniquePairs xs = [ SP x y | (x:ys) <- nonEmptyTails xs, y <- ys ]

nonEmptyTails :: [a] -> [[a]]
nonEmptyTails = L.init . L.tails


data ST a b c = ST !a !b !c deriving (Eq,Ord,Show)

-- | All unieuqe unordered triplets.
uniqueTriplets    :: [a] -> [ST a a a]
uniqueTriplets xs = [ ST x y z | (x:ys) <- nonEmptyTails xs, SP y z <- uniquePairs ys]
