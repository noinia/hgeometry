{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE QuantifiedConstraints #-}
module HGeometry.Cyclic
  ( Cyclic(..)
  , toCircularVector
  , HasDirectedTraversals(..)
  , isShiftOf
  ) where

--------------------------------------------------------------------------------

import           Control.DeepSeq (NFData)
import           Control.Lens
import           Control.Monad (forM_)
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Maybe (isJust)
import           Data.Semigroup.Foldable
import qualified Data.Vector as V
import           Data.Vector.Circular (CircularVector(..))
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.NonEmpty as NV
import           GHC.Generics (Generic)
import           HGeometry.Foldable.Util
import           HGeometry.StringSearch.KMP (isSubStringOf)
import           HGeometry.Vector.NonEmpty.Util ()
--------------------------------------------------------------------------------

-- | A cyclic sequence type
newtype Cyclic v a = Cyclic (v a)
 deriving newtype (Functor,Foldable,NFData,Eq)
 deriving stock (Generic)
-- not sure if we want this Eq instance or not .

instance Foldable1 v    => Foldable1    (Cyclic v)

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
  ix i = \f (Cyclic v) -> let n = F.length v
                          in Cyclic <$> ix (i `mod` n) f v

-- | Turn the cyclic vector into a circular Vector
toCircularVector            :: Cyclic NV.NonEmptyVector a -> CircularVector a
toCircularVector (Cyclic v) = CircularVector v 0


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
isShiftOf         :: (Eq a, Foldable1 v) => Cyclic v a -> Cyclic v a -> Bool
xs `isShiftOf` ys = let twice zs     = let zs' = leftElements zs in zs' <> zs'
                        once         = leftElements
                        leftElements = NV.fromNonEmpty . toNonEmpty
                        check as bs  = isJust $ once as `isSubStringOf` twice bs
                    in length xs == length ys && check xs ys
