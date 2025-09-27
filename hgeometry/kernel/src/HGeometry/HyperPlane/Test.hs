{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module HGeometry.HyperPlane.Test where

import           Control.Lens hiding (cons, snoc, uncons, unsnoc)
import           Data.Type.Ord
import           GHC.TypeLits
import           HGeometry.Properties
import           HGeometry.Vector
import           HGeometry.Point.Class(Point_(..), HasVector(..))
import           Prelude hiding (last)

--------------------------------------------------------------------------------

-- newtype Point d r =  Point (Vector d r)

-- type instance Dimension (Point d r) = d
-- type instance NumType (Point d r) = r


-- class (HasVector point point
--       , Dimension point ~ d, NumType point ~ r
--       ) => Point_ point d r | point -> d, point -> r where


--------------------------------------------------------------------------------

-- | Access the last element of a vector
last :: forall vector d r. (Vector_ vector d r, 1 <= d) => IndexedLens' Int vector r
last = component @(d-1)

-- | Constraints on d needed to be able to construct hyperplanes; pretty much all of
-- these are satisfied by default, it is just that the typechecker does not realize that.
type MkHyperPlaneConstraints d r =
  ( d < d+1, KnownNat d
  , Has_ Metric_ d r, Has_ Metric_ (d+1) r
  , Has_ Vector_ d r, Has_ Vector_ (d+1) r
  )

--------------------------------------------------------------------------------


newtype NonVerticalHyperPlane d r = NonVerticalHyperPlane (Vector d r)

type instance NumType   (NonVerticalHyperPlane d r) = r
type instance Dimension (NonVerticalHyperPlane d r) = d

--------------------------------------------------------------------------------

instance ( MkHyperPlaneConstraints d r, 1 + (d-1) ~ d, Has_ Additive_ (d-1) r
         , Num r
         , 2 <= d
         ) => NonVerticalHyperPlane_ (NonVerticalHyperPlane d r) d r where

--------------------------------------------------------------------------------

-- -- | A class to represent hyperplanes in d-dimensional space.
-- class ( NumType hyperPlane ~ r
--       , Dimension hyperPlane ~ d
--       , Has_ Vector_ d r
--       , Has_ Vector_ (1+d) r
--       -- , NumType (EquationFor hyperPlane) ~ r
--       ) => HyperPlane_ hyperPlane d r | hyperPlane -> d
--                                       , hyperPlane -> r where


--------------------------------------------------------------------------------
-- | Non-vertical hyperplanes.
class NonVerticalHyperPlane_ hyperPlane d r where
