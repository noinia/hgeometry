{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Polygon.Simple.Sample
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Newtype to wrap some foldable structure promissing it is non-empty.
--
--------------------------------------------------------------------------------
module HGeometry.Foldable.NonEmpty
  ( NonEmptyF(..)
  ) where

import           Control.Lens
import           Data.Foldable1
import           Data.Functor.Apply (Apply, (<.*>), MaybeApply(..))
import           Data.Semigroup.Traversable
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | We promise that the structure is non-empty
newtype NonEmptyF f a = NonEmptyF (f a)
  deriving newtype (Functor,Foldable)
  deriving Generic

_NonEmptyF :: Iso (NonEmptyF f a) (NonEmptyF f b) (f a) (f b)
_NonEmptyF = coerced

instance Traversable f => Traversable (NonEmptyF f)  where
  traverse f (NonEmptyF s) = NonEmptyF <$> traverse f s

type instance Index   (NonEmptyF f a) = Index   (f a)
type instance IxValue (NonEmptyF f a) = IxValue (f a)

instance Ixed (f a) => Ixed (NonEmptyF f a) where
  ix i = _NonEmptyF . ix i

instance Wrapped   (NonEmptyF f a)

instance Rewrapped (NonEmptyF f a) (NonEmptyF f b)

instance FunctorWithIndex i f => FunctorWithIndex i (NonEmptyF f) where
  imap f (NonEmptyF s) = NonEmptyF $ imap f s

instance FoldableWithIndex i f => FoldableWithIndex i (NonEmptyF f) where
  ifoldMap f (NonEmptyF s) = ifoldMap f s

instance TraversableWithIndex i f => TraversableWithIndex i (NonEmptyF f) where
  itraverse f (NonEmptyF s) = NonEmptyF <$> itraverse f s

-- instance Foldable1 (NonEmptyF f) where
--   foldMap1 = foldMap1Default

-- instance Traversable1 (NonEmptyF f)  where
--   traverse1 f (NonEmptyF s) = NonEmptyF f  <$> traverse1Seq f s

-- instance HasFromFoldable1 (NonEmptyF f)  where
--   fromNonEmpty = Seq1 . fromNonEmpty

-- --------------------------------------------------------------------------------
-- -- * Instances specific to Seq

-- instance Traversable1 (NonEmptyF Seq)  where
--   traverse1 f (NonEmptyF s) = NonEmptyF <$> traverse1Seq f s

-- instance Foldable1 (NonEmptyF Seq) where
--   foldMap1 = foldMap1Default



-- -- | Traverse a non-empty sequence
-- traverse1Seq   :: Apply f => (a -> f b) -> Seq a -> f (Seq b)
-- traverse1Seq f = \case
--   Seq.Empty -> error "traverse1Seq: precondition violated"
--   (x :<| s) -> (:<|) <$> f x <.*> traverse1Maybe f s
