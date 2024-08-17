{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Cyclic
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Representing Cyclic Sequences
--
--------------------------------------------------------------------------------
module HGeometry.Cyclic
  ( Cyclic(..)
  , toCircularVector
  , HasDirectedTraversals(..)
  , ShiftedEq(..)
  ) where

--------------------------------------------------------------------------------

import           Control.DeepSeq (NFData)
import           Control.Lens
import           Control.Monad (forM_)
import qualified Data.Foldable as F
import           Data.Functor.Apply (Apply, (<.*>), (<*.>), MaybeApply(..))
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (isJust)
import           Data.Semigroup.Foldable
import qualified Data.Vector as V
import           Data.Vector.Circular (CircularVector(..))
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.NonEmpty as NV
import           GHC.Generics (Generic)
import           HGeometry.Foldable.Util
import           HGeometry.Sequence.NonEmpty
import           HGeometry.StringSearch.KMP (isSubStringOf)
import           HGeometry.Vector.NonEmpty.Util ()

--------------------------------------------------------------------------------

-- | A cyclic sequence type
newtype Cyclic v a = Cyclic (v a)
 deriving newtype (Functor,Foldable,Foldable1,NFData,Eq)
 deriving stock (Generic)
-- not sure if we want this Eq instance or not .


instance Traversable1 v => Traversable1 (Cyclic v) where
  traverse1 f (Cyclic v) = Cyclic <$> traverse1 f v
instance Traversable v => Traversable (Cyclic v) where
  traverse f (Cyclic v) = Cyclic <$> traverse f v


instance FunctorWithIndex i v => FunctorWithIndex i (Cyclic v) where
  imap f (Cyclic v) = Cyclic $ imap f v
instance FoldableWithIndex i v => FoldableWithIndex i (Cyclic v) where
  ifoldMap f (Cyclic v) = ifoldMap f v
instance TraversableWithIndex i v => TraversableWithIndex i (Cyclic v) where
  itraverse f (Cyclic v) = Cyclic <$> itraverse f v

instance HasFromFoldable v => HasFromFoldable (Cyclic v)  where
  fromFoldable = Cyclic . fromFoldable
  fromList = Cyclic . fromList

instance HasFromFoldable1 v => HasFromFoldable1 (Cyclic v)  where
  fromFoldable1 = Cyclic . fromFoldable1
  fromNonEmpty  = Cyclic . fromNonEmpty

type instance Index   (Cyclic v a) = Index   (v a)
type instance IxValue (Cyclic v a) = IxValue (v a)

instance (Index (v a) ~ Int, Foldable v, Ixed (v a)) => Ixed (Cyclic v a) where
  ix i = \f (Cyclic v) -> let n  = F.length v
                          in Cyclic <$> ix (i `mod` n) f v

-- | Turn the cyclic vector into a circular Vector
toCircularVector            :: Cyclic NV.NonEmptyVector a -> CircularVector a
toCircularVector (Cyclic v) = CircularVector v 0


-- | Class that models that some type has a cyclic traversal starting
-- from a particular index.
class HasDirectedTraversals v where
  -- | A rightward-traversal over all elements starting from the given one.
  --
  -- running time : \(O(n)\)
  traverseRightFrom          :: Index (v a) -> IndexedTraversal1' (Index (v a)) (v a) a

  -- | A rightward-traversal over all elements starting from the given one.
  --
  -- running time : \(O(n)\)
  traverseLeftFrom          :: Index (v a) -> IndexedTraversal1' (Index (v a)) (v a) a

instance HasDirectedTraversals v => HasDirectedTraversals (Cyclic v) where
  traverseRightFrom s paFa (Cyclic v) = Cyclic <$> traverseRightFrom s paFa v
  traverseLeftFrom  s paFa (Cyclic v) = Cyclic <$> traverseLeftFrom  s paFa v

instance HasDirectedTraversals NV.NonEmptyVector where
  traverseRightFrom s paFa v = traverseByOrder indices' paFa v
    where
      n        = F.length v
      indices' = NonEmpty.fromList [s..(s+n-1)]

  traverseLeftFrom s paFa v = traverseByOrder indices' paFa v
    where
      n        = F.length v
      indices' = NonEmpty.fromList [s,s-1..(s-n+1)]

-- | Helper to build traversal1's
wrapMaybeApply :: (Indexable Int p, Apply f) => p a (f b) -> p a (MaybeApply f b)
wrapMaybeApply = rmap (MaybeApply . Left)


instance HasDirectedTraversals ViewL1 where
  traverseRightFrom i = \paFb xs -> let i'    = i `mod` F.length xs
                                        paFb' = wrapMaybeApply paFb
                                        combine (x :<< sa) sb = x :<< (sa <> sb)
                                    in case splitL1At i' xs of
      Nothing            -> traversed1 paFb xs
      Just (pref,x,suff) -> combine <$>  reindexed (+i') traversed1 paFb (x :<< suff)
                                    <.*> itraversed paFb' pref

  traverseLeftFrom i = \paFb xs ->
                         let i'    = i `mod` F.length xs
                             paFb' = wrapMaybeApply paFb
                             combine r1 rs = viewl1 $ r1 <>> rs
                         in case splitL1At i' xs of
      Nothing            -> backwards traversed1 paFb xs
      Just (pref,x,suff) -> combine <$>  backwards traversed1 paFb (pref :>> x)
                                    <.*> backwards (reindexed (\j -> 1 + i' + j) itraversed)
                                                   paFb' suff

-- | traverse the vector in the given order
traverseByOrder                 :: NonEmpty.NonEmpty Int
                                -> IndexedTraversal1' Int (NV.NonEmptyVector a) a
traverseByOrder indices' paFa v = build <$> xs
  where
    n = F.length v
    xs = traverse1 (\i' -> let i = i' `mod` n
                           in (i,) <$> indexed paFa i (v NV.! i)
                   ) indices'
    build ys = NV.unsafeFromVector $ V.create $ do
                   v' <- MV.unsafeNew n
                   forM_ ys $ \(i,y) ->
                     MV.write v' i y
                   pure v'

-- | Test if the circular list is a cyclic shift of the second
-- list.
--
-- Running time: \(O(n+m)\), where \(n\) and \(m\) are the sizes of
-- the lists.
isShiftOfI         :: (Eq a, Foldable1 v) => Cyclic v a -> Cyclic v a -> Bool
xs `isShiftOfI` ys = let twice zs     = let zs' = leftElements zs in zs' <> zs'
                         once         = leftElements
                         leftElements = NV.fromNonEmpty . toNonEmpty
                         check as bs  = isJust $ once as `isSubStringOf` twice bs
                     in length xs == length ys && check xs ys

--------------------------------------------------------------------------------

-- | Class for types that have an Equality test up to cyclic shifts
class ShiftedEq t where
  -- | The type of the elements stored in this cyclic container.
  type ElemCyclic t
  -- | Given a and b, test if a is a shifted version of the other.
  isShiftOf :: Eq (ElemCyclic t) => t -> t -> Bool

  -- TODO: It may be nice to have a version that actually returns the index, if it is a
  -- shift.

instance Foldable1 v => ShiftedEq (Cyclic v a) where
  type ElemCyclic (Cyclic v a) = a
  -- ^ runs in linear time in the inputs.
  isShiftOf = isShiftOfI
