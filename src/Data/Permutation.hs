{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Permutation
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for representing a Permutation
--
--------------------------------------------------------------------------------
module Data.Permutation where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad (forM)
import           Control.Monad.ST (runST)
import qualified Data.Foldable as F
import           Data.Maybe (catMaybes)
import qualified Data.Traversable as T
import qualified Data.Vector as V
import qualified Data.Vector.Generic as GV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | Orbits (Cycles) are represented by vectors
type Orbit a = V.Vector a

-- | Cyclic representation of a permutation.
data Permutation a = Permutation { _orbits  :: V.Vector (Orbit a)
                                 , _indexes :: UV.Vector (Int,Int)
                                               -- ^ idxes (fromEnum a) = (i,j)
                                               -- implies that a is the j^th
                                               -- item in the i^th orbit
                                 }
                   deriving (Show,Eq,Generic)
makeLenses ''Permutation

instance NFData a => NFData (Permutation a)

instance Functor Permutation where
  fmap = T.fmapDefault

instance F.Foldable Permutation where
  foldMap = T.foldMapDefault

instance T.Traversable Permutation where
  traverse f (Permutation os is) = flip Permutation is <$> T.traverse (T.traverse f) os


elems :: Permutation a -> V.Vector a
elems = GV.concat . GV.toList . _orbits

size      :: Permutation a -> Int
size perm = GV.length (perm^.indexes)

-- | The cycle containing a given item
cycleOf        :: Enum a => Permutation a -> a -> Orbit a
cycleOf perm x = perm^?!orbits.ix (perm^?!indexes.ix (fromEnum x)._1)


-- | Next item in a cyclic permutation
next     :: GV.Vector v a => v a -> Int -> a
next v i = let n = GV.length v in v GV.! ((i+1) `mod` n)

-- | Previous item in a cyclic permutation
previous     :: GV.Vector v a => v a -> Int -> a
previous v i = let n = GV.length v in v GV.! ((i-1) `mod` n)

-- | Lookup the indices of an element, i.e. in which orbit the item is, and the
-- index within the orbit.
--
-- runnign time: \(O(1)\)
lookupIdx        :: Enum a => Permutation a -> a -> (Int,Int)
lookupIdx perm x = perm^?!indexes.ix (fromEnum x)

-- | Apply the permutation, i.e. consider the permutation as a function.
apply        :: Enum a => Permutation a -> a -> a
apply perm x = let (c,i) = lookupIdx perm x
               in next (perm^?!orbits.ix c) i


-- | Find the cycle in the permutation starting at element s
orbitFrom     :: Eq a => a -> (a -> a) -> [a]
orbitFrom s p = s : (takeWhile (/= s) . tail $ iterate p s)

-- | Given a vector with items in the permutation, and a permutation (by its
-- functional representation) construct the cyclic representation of the
-- permutation.
cycleRep        :: (GV.Vector v a, Enum a, Eq a) => v a -> (a -> a) -> Permutation a
cycleRep v perm = toCycleRep n $ runST $ do
    bv    <- UMV.replicate n False -- bit vector of marks
    morbs <- forM [0..(n - 1)] $ \i -> do
               m <- UMV.read bv (fromEnum $ v GV.! i)
               if m then pure Nothing -- already visited
                    else do
                      let xs = orbitFrom (v GV.! i) perm
                      markAll bv $ map fromEnum xs
                      pure . Just $ xs
    pure . catMaybes $ morbs
  where
    n  = GV.length v

    mark    bv i = UMV.write bv i True
    markAll bv   = mapM_ (mark bv)


-- | Given the size n, and a list of Cycles, turns the cycles into a
-- cyclic representation of the Permutation.
toCycleRep      :: Enum a => Int -> [[a]] -> Permutation a
toCycleRep n os = Permutation (V.fromList . map V.fromList $ os) (genIndexes n os)


genIndexes      :: Enum a => Int -> [[a]] -> UV.Vector (Int,Int)
genIndexes n os = UV.create $ do
                                v <- UMV.new n
                                mapM_ (uncurry $ UMV.write v) ixes'
                                pure v
  where
    f i c = zipWith (\x j -> (fromEnum x,(i,j))) c [0..]
    ixes' = concat $ zipWith f [0..] os
