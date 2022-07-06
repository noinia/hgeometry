--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.SoS.Point
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Points whose coordinates have been symbolically perturbed.
--
--------------------------------------------------------------------------------
module Geometry.SoS.Point
  ( toSymbolic, fromSymbolic
  , toSoSRational, fromSoSRational
  , SoSI
  ) where

import Control.Lens hiding (Index)
import Data.Indexed
import Data.Ratio.Generalized
import Data.RealNumber.Symbolic
import Geometry.Point.Class
-- import Geometry.Point.Internal
-- import Geometry.Vector
import Test.QuickCheck (Arbitrary(..))

--------------------------------------------------------------------------------

-- | Given an input point, transform its number type to include
-- symbolic $\varepsilon$ expressions so that we can use SoS.
toSymbolic    :: ( Point point d r, Functor (point d)
                 , HasIndex (point d r), Num r
                 )
              => point d r -> point d (Symbolic SoSI r)
toSymbolic p = let i = sosIndex p
               in p&asVector %~ imap (\j x -> perturb x $ MkSoS i j)

-- TODO: I think I may want to choose a concrete point implementation for the symbolic ones

-- | Drops the pertubations in a point
fromSymbolic :: (Functor f, Num r) => f (Symbolic i r) -> f r
fromSymbolic = fmap roundToConstant

----------------------------------------

-- | Constructs an point whose numeric type uses SoSRational, so that
-- we can use SoS.
toSoSRational :: ( Point point d r, Functor (point d)
                 , HasIndex (point d r), Eq r, Num r)
              => point d r -> point d (SoSRational SoSI r)
toSoSRational = fmap (\x -> sosRational x 1) . toSymbolic

-- | Drops the pertubations
fromSoSRational :: (Functor f, Fractional r) => f (SoSRational i r) -> f r
fromSoSRational = fmap (\x -> roundToConstant (numerator x) / roundToConstant (denominator x))

--------------------------------------------------------------------------------

-- | the index type used to disambiguate the values
data SoSI = MkSoS {-# UNPACK #-}!Index -- ^ original index
                  {-# UNPACK #-}!Int -- ^ index of the coordinate in [0..(d-1)]
          deriving (Show,Eq,Ord)

-- for now I've kept the two components separtely, as to avoid blowing
-- up the range required for the indices. Maybe it would be faster to
-- just map the jth coordinate of point p_i to index i*d+j
-- though. That way we can map to a Point d (Symoblic (WithIndex
-- r)). Maybe that way we can use IntSets and so on to represent the
-- Bags/Symbolic type rather than the arbitrary i as we currently
-- have.

instance Arbitrary SoSI where
  arbitrary = MkSoS <$> arbitrary <*> arbitrary
