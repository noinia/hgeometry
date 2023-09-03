{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.HalfLine
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Half-lines in \(d\)-dimensional space.
--
--------------------------------------------------------------------------------
module HGeometry.HalfLine
  ( HalfLine(MkHalfLine,HalfLine)
  ) where

import GHC.TypeLits
import HGeometry.Line.PointAndVector
import HGeometry.Point
import HGeometry.Vector
import HGeometry.Properties
import Text.Read

--------------------------------------------------------------------------------

-- | A Halfline in R^d
newtype HalfLine d r = MkHalfLine (LinePV d r)

type instance Dimension (HalfLine d r) = d
type instance NumType   (HalfLine d r) = r

-- | Construct a Halfline from its starting point and the vector
pattern HalfLine     :: Point d r -> Vector d r -> HalfLine d r
pattern HalfLine p v = MkHalfLine (LinePV p v)
{-# COMPLETE HalfLine #-}

deriving newtype instance Eq (LinePV d r) => Eq (HalfLine d r)

instance ( Show r, KnownNat d, Has_ Additive_ d r
         ) => Show (HalfLine d r) where
  showsPrec k (HalfLine p v) = showParen (k > appPrec) $
                                 showString "HalfLine "
                               . showsPrec (appPrec+1) p
                               . showChar ' '
                               . showsPrec (appPrec+1) v

appPrec :: Int
appPrec = 10

instance (Read r, Has_ Additive_ d r, KnownNat d
         ) => Read (HalfLine d r) where
  readPrec = parens (prec appPrec $ do
                        Ident "HalfLine" <- lexP
                        p <- step readPrec
                        v <- step readPrec
                        pure (HalfLine p v))
