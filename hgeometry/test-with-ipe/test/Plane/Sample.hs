module Plane.Sample
  ( Sample(..)
  , sampleSubset
  ) where

import System.Random
import Data.Set qualified as Set
import Data.Set.NonEmpty qualified as NESet
import Data.These
--------------------------------------------------------------------------------

data Sample sample a = Sample { sampled   :: sample a
                              , sampleSize :: {-#UNPACK#-}!Int
                              , remaining :: Set.Set a
                              , totalSize :: {-#UNPACK#-}!Int
                                -- i.e. length sampled + length remaining
                              }
                     deriving Show

instance Foldable sample => Foldable (Sample sample) where
  foldMap f (Sample xs _ ys _) = foldMap f xs <> foldMap f ys
  length = totalSize

-- | Given a parameter r, the main idea is to take a (r/n)-sample of
-- the given set; we return both the sampled set, as well as the
-- remainder (non-sampled elements).
--
-- pre: 3 <= r <= n
--
-- more specifically, we return the first three elements, so that the
-- ouput set has size at least 3, and a p-sample of the rest.
sampleSubset            :: (Ord a, RandomGen gen)
                        => gen
                        -> Int -- ^ the desired sample size: pre >= 1
                        -> Int -- ^ the total size of the set
                        -> NESet.NESet a
                        -> (Sample NESet.NESet a, gen)
sampleSubset gen r n xs = case NESet.splitAt r xs of
   These sample rest -> (Sample sample r (NESet.toSet rest) n, gen)
   _                 -> (Sample xs r Set.empty n, gen)


  -- (Sample (NonEmpty.fromList rs) r rest n, gen') -- the from list is safe since by precondition r >= 1
  -- where
  --   NESet.splitAt

  --   (rs,rest) = List.splitAt r (toList xs)
    -- gen'      = gen  -- TODO
  -- FIXME: do the actual sampling rather than just returning the first r elents
