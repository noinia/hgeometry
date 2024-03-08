--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Permutation
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Data type for representing a non-empty Permutation
--
--------------------------------------------------------------------------------
module HGeometry.Permutation
  ( Permutation(Permutation)
  , orbits
  , indices

  , Orbit
  , elems
  , size
  , cycleOf
  , next, previous
  , lookupIdx
  , apply
  , orbitFrom

  , cycleRep, toCycleRep
  ) where

import           Control.DeepSeq
import           Control.Lens
import           Control.Monad (forM)
import           Control.Monad.ST (runST)
import qualified Data.Foldable as F
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (catMaybes)
import           Data.Vector.NonEmpty (NonEmptyVector)
import qualified Data.Vector.NonEmpty as NonEmptyV
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Unboxed.Mutable as UMV
import           GHC.Generics (Generic)
import           HGeometry.Vector.NonEmpty.Util ()

--------------------------------------------------------------------------------

-- | Orbits (Cycles) are represented by vectors
type Orbit a = NonEmptyVector a

-- | Cyclic representation of a non-empty permutation.
data Permutation a = Permutation { _orbits  :: NonEmptyVector (Orbit a)
                                 , _indexes :: UV.Vector (Int,Int)
                                               -- ^ idxes (fromEnum a) = (i,j)
                                               -- implies that a is the j^th
                                               -- item in the i^th orbit
                                 }
                   deriving (Show,Eq,Generic,Functor,Foldable)

-- | Lens to access the orbits of the permutation
orbits :: Lens (Permutation a) (Permutation b) (NonEmptyVector (Orbit a)) (NonEmptyVector (Orbit b))
orbits = lens _orbits (\p os -> p { _orbits = os })

-- | Lens to access the indexes of the permutation.
--
-- idxes (fromEnum a) = (i,j) implies that a is the j^th item in the
-- i^th orbit
indexes :: Lens' (Permutation a) (UV.Vector (Int,Int))
indexes = lens _indexes (\p ixs -> p { _indexes = ixs })


instance NFData a => NFData (Permutation a)

instance Traversable Permutation where
  traverse f (Permutation os is) = flip Permutation is <$> traverse (traverse f) os

-- | Get the elements of the permutation
elems :: Permutation a -> NonEmptyVector a
elems = NonEmptyV.concatMap id . _orbits

-- | Get the size of a permutation
--
-- running time: \(O(1)\)
size      :: Permutation a -> Int
size perm = UV.length (perm^.indexes)

-- | The cycle containing a given item
cycleOf        :: Enum a => Permutation a -> a -> Orbit a
cycleOf perm x = perm^?!orbits.ix (perm^?!indexes.ix (fromEnum x)._1)


-- | Next item in a cyclic permutation
next     :: NonEmptyVector a -> Int -> a
next v i = let n = NonEmptyV.length v in v NonEmptyV.! ((i+1) `mod` n)

-- | Previous item in a cyclic permutation
previous     :: NonEmptyVector a -> Int -> a
previous v i = let n = NonEmptyV.length v in v NonEmptyV.! ((i-1) `mod` n)

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
orbitFrom     :: Eq a => a -> (a -> a) -> NonEmpty a
orbitFrom s p = s :| (takeWhile (/= s) . NonEmpty.tail $ NonEmpty.iterate p s)

-- | Given a vector with items in the permutation, and a permutation (by its
-- functional representation) construct the cyclic representation of the
-- permutation.
cycleRep        :: (Enum a, Eq a) => NonEmptyVector a -> (a -> a) -> Permutation a
cycleRep v perm = toCycleRep n $ runST $ do
    bv    <- UMV.replicate n False -- bit vector of marks
    morbs <- forM [0..(n - 1)] $ \i -> do
               m <- UMV.read bv (fromEnum $ v NonEmptyV.! i)
               if m then pure Nothing -- already visited
                    else do
                      let xs = orbitFrom (v NonEmptyV.! i) perm
                      markAll bv $ fmap fromEnum xs
                      pure . Just $ xs
    pure . NonEmpty.fromList . catMaybes $ morbs
  where
    n  = NonEmptyV.length v

    mark    bv i = UMV.write bv i True
    markAll bv   = mapM_ (mark bv)

-- | Given the size n, and a list of Cycles, turns the cycles into a
-- cyclic representation of the Permutation.
toCycleRep      :: Enum a => Int -> NonEmpty (NonEmpty a) -> Permutation a
toCycleRep n os = Permutation (NonEmptyV.fromNonEmpty . fmap NonEmptyV.fromNonEmpty $ os)
                              (genIndexes n os)

-- | Helper function to generate the indices of a permutation
genIndexes      :: Enum a => Int -> NonEmpty (NonEmpty a)  -> UV.Vector (Int,Int)
genIndexes n os = UV.create $ do
                                v <- UMV.new n
                                mapM_ (uncurry $ UMV.write v) ixes'
                                pure v
  where
    f i c = zipWith (\x j -> (fromEnum x,(i,j))) (F.toList c) [0..]
    ixes' = concat $ zipWith f [0..] (F.toList os)
