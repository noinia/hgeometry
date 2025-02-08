{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Point.Either
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A type that is isomorphic to Either pointA pointB, and its isPoint instance
-- provided that pointA and pointB are actually points as well.
--
--------------------------------------------------------------------------------
module HGeometry.Point.Either
  ( OriginalOrExtra(..)
  ) where

import Control.Lens
import Data.Bifoldable
import Data.Bitraversable
import HGeometry.Point.Class
import HGeometry.Properties

--------------------------------------------------------------------------------

-- | Helper type for distinguishing original vertices from extra ones.
data OriginalOrExtra orig extra = Original orig
                                | Extra    extra
                                deriving (Show,Eq,Functor)

instance Bifunctor OriginalOrExtra where
  bimap f g = \case
    Original p -> Original (f p)
    Extra p    -> Extra (g p)

instance Bifoldable OriginalOrExtra where
  bifoldMap f g = \case
    Original p -> f p
    Extra p    -> g p

instance Bitraversable OriginalOrExtra where
  bitraverse f g = \case
    Original p -> Original <$> f p
    Extra p    -> Extra <$> g p


type instance NumType   (OriginalOrExtra orig extra) = NumType orig
type instance Dimension (OriginalOrExtra orig extra) = Dimension orig

instance ( HasVector orig orig, HasVector extra extra
         , HasVector orig orig', HasVector extra extra'
         , Dimension extra ~ Dimension orig, NumType extra ~ NumType orig
         , Dimension extra' ~ Dimension orig', NumType extra' ~ NumType orig'
         ) => HasVector (OriginalOrExtra orig extra) (OriginalOrExtra orig' extra') where
  vector = lens g (flip s)
    where
      g = \case
        Original o -> o^.vector
        Extra    e -> e^.vector
      s v = \case
        Original o -> Original (o&vector .~ v)
        Extra    e -> Extra    (e&vector .~ v)


instance ( HasCoordinates orig orig', HasCoordinates extra extra'
         , HasVector orig orig, HasVector extra extra
         , Dimension extra ~ Dimension orig, NumType extra ~ NumType orig
         , Dimension extra' ~ Dimension orig', NumType extra' ~ NumType orig'
         ) => HasCoordinates (OriginalOrExtra orig extra) (OriginalOrExtra orig' extra')

instance (Affine_ orig d r, Affine_ extra d r) => Affine_ (OriginalOrExtra orig extra) d r

instance (Point_ orig d r, Point_ extra d r)   => Point_  (OriginalOrExtra orig extra) d r
