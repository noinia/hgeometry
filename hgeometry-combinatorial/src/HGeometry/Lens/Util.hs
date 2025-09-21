--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Lens.Util
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Some helper utils for Lens
--
--------------------------------------------------------------------------------
module HGeometry.Lens.Util
  ( folding1
  , ifolding1
  , itoNonEmptyOf

  , toVectorOf
  , toNonEmptyVectorOf
  ) where

import           Control.Lens
import           Control.Lens.Internal.Fold (NonEmptyDList(..))
import           Data.Foldable1
import           Data.Functor.Apply (Apply)
import           Data.Functor.Contravariant (phantom)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Semigroup.Foldable
import qualified VectorBuilder.Builder as Builder
import qualified VectorBuilder.Vector as Builder
import           Data.Vector.NonEmpty (NonEmptyVector)
import           Data.Vector (Vector)
import qualified Data.Vector.NonEmpty as NonEmptyV

--------------------------------------------------------------------------------

-- TODO: upstream these to lens
-- taken and modified directly from lens

-- | construct a Fold1 from a function that produces a Foldable1
folding1         :: Foldable1 f => (s -> f a) -> Fold1 s a
folding1 sfa agb = phantom . traverse1_ agb . sfa
{-# INLINE folding1 #-}

-- | Version of ifolding to build an 'IndexedFold1'
ifolding1       :: (Foldable1 f, Indexable i p, Contravariant g, Apply g)
                => (s -> f (i, a)) -> Over p g s t a b
ifolding1 sfa f = phantom . traverse1_ (phantom . uncurry (indexed f)) . sfa
{-# INLINE ifolding1 #-}

-- | indexed version of 'toNonEmptyOf'
itoNonEmptyOf   :: IndexedGetting i (NonEmptyDList (i,a)) s a -> s -> NonEmpty (i,a)
itoNonEmptyOf l = flip getNonEmptyDList [] . ifoldMapOf l (\i a -> NonEmptyDList ((i,a) :|))
{-# INLINE itoNonEmptyOf #-}

--------------------------------------------------------------------------------

-- | Given a fold, construct a non-emptyVector out of it
toNonEmptyVectorOf   :: Getting (Builder.Builder a) s a -> s -> NonEmptyVector a
toNonEmptyVectorOf l = NonEmptyV.unsafeFromVector . toVectorOf l

-- | Given a fold, construct a non-emptyVector out of it
toVectorOf     :: Getting (Builder.Builder a) s a -> s -> Vector a
toVectorOf l s = Builder.build $ foldMapOf l Builder.singleton s
