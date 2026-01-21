module Plane.Sample
  ( Sample(..)
  ) where

import Data.Foldable
import System.Random
import Data.Foldable1
import Data.List.NonEmpty qualified as NonEmpty
import Data.List qualified as List
import Data.List.NonEmpty (NonEmpty(..))

--------------------------------------------------------------------------------

data Sample sample a = Sample { sampled   :: sample a
                              , sampleSize :: {-#UNPACK#-}!Int
                              , remaining :: [a]
                              , totalSize :: {-#UNPACK#-}!Int
                                -- i.e. length sampled + length remaining
                              }
                     deriving Show


-- | Given a parameter r, the main idea is to take a (r/n)-sample of
-- the given set; we return both the sampled set, as well as the
-- remainder (non-sampled elements).
--
-- pre: r >= 3
--
-- more specifically, we return the first three elements, so that the
-- ouput set has size at least 3, and a p-sample of the rest.
sampleSubset            :: (Foldable1 set, RandomGen gen)
                        => gen
                        -> Int -- ^ the desired sample size: pre >= 1
                        -> Int -- ^ the total size of the set
                        -> set a
                        -> (Sample NonEmpty a, gen)
sampleSubset gen r n xs = (Sample (NonEmpty.fromList rs) r rest n, gen') -- the from list is safe since by precondition r >= 1
  where
    (rs,rest) = List.splitAt r (toList xs)
    gen'      = gen  -- TODO
  -- FIXME: do the actual sampling rather than just returning the first r elents
