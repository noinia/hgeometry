module Geometry.SoS.Point
  ( toSymbolic
  , toSoSRational

  , SoSI
  ) where

import Control.Lens hiding (Index)
import Data.Indexed
import Data.RealNumber.Symbolic
import Geometry.Point.Class
import Geometry.Point.Internal
import Geometry.Vector
import Test.QuickCheck (Arbitrary(..))

--------------------------------------------------------------------------------


-- | Given an input point, transform its number type to include
-- symbolic $\varepsilon$ expressions so that we can use SoS.
toSymbolic    :: (Arity d, ToAPoint point d r, HasIndex point)
              => point -> Point d (Symbolic SoSI r)
toSymbolic p' = let p = p'^.toPoint
                    i = sosIndex p'
                in p&vector %~ imap (\j x -> symbolic x $ MkSoS i j)

-- | Constructs an point whose numeric type uses SoSRational, so that
-- we can use SoS.
toSoSRational :: (Arity d, ToAPoint point d r, HasIndex point, Eq r, Num r)
              => point -> Point d (SoSRational SoSI r)
toSoSRational = fmap (\x -> sosRational x 1) . toSymbolic

-- instance (ToAPoint point d r, Arity d, HasIndex point)
--          => ToAPoint (WithSoS point) d (Symbolic SoSI r) where
--   toPoint = to undefined -- toSymbolic


-- FIXME: I guess the indices should be strictly positive, otherwise some term eps(0) would add 1?


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
