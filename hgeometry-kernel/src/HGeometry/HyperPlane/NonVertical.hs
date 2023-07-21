{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.HyperPlane.NonVertical
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Non-vertical hyperplanes in d-dimensional space.
--
--------------------------------------------------------------------------------
module HGeometry.HyperPlane.NonVertical
  ( NonVerticalHyperPlane(NonVerticalHyperPlane, Plane)
  , asNonVerticalHyperPlane
  , Plane
  , Plane_, pattern Plane_
  ) where

import           Control.Lens hiding (snoc, uncons)
import qualified Data.Foldable as F
import           Data.Functor.Classes
import           Data.Type.Ord
import           GHC.TypeLits
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.Internal (MkHyperPlaneConstraints)
import           HGeometry.Properties
import           HGeometry.Vector
import           Prelude hiding (last)
import           Text.Read (Read (..), readListPrecDefault)

--------------------------------------------------------------------------------

-- $setup
-- >>> let myHyperPlane = NonVerticalHyperPlane $ Vector2 1 2
--

-- | A non-vertical Hyperplane described by \( x_d = a_d + \sum_{i=1}^{d-1}
-- a_i * x_i \) where \(\langle a_1,..,a_d \rangle \) are the
-- coefficients of te hyperplane.
--
--
-- e.g. the 'myHyperPlane' defines the hyperplane described by
--
-- y = 2 + 1*x
--
newtype NonVerticalHyperPlane d r = NonVerticalHyperPlane (Vector d r)

type instance NumType   (NonVerticalHyperPlane d r) = r
type instance Dimension (NonVerticalHyperPlane d r) = d

deriving stock instance Eq  (Vector d r) => Eq (NonVerticalHyperPlane d r)
deriving stock instance Ord (Vector d r) => Ord (NonVerticalHyperPlane d r)

deriving stock instance Functor     (Vector d) => Functor     (NonVerticalHyperPlane d)
deriving stock instance Foldable    (Vector d) => Foldable    (NonVerticalHyperPlane d)
deriving stock instance Traversable (Vector d) => Traversable (NonVerticalHyperPlane d)

instance (Show r, Foldable (Vector d)) => Show (NonVerticalHyperPlane d r) where
  showsPrec k (NonVerticalHyperPlane v) = showParen (k > app_prec) $
                                            showString "NonVerticalHyperPlane " .
                                            showsPrec 11 (F.toList v)
    where
      app_prec = 10

instance ( Read r, Has_ Vector_ d r) => Read (NonVerticalHyperPlane d r) where
  readPrec = readData $ readUnaryWith parseVec "NonVerticalHyperPlane" NonVerticalHyperPlane
    where
      parseVec = do lst <- readPrec
                    case vectorFromList @(Vector d r) lst of
                      Just v -> pure v
                      _      -> fail "NonVerticalHyperPlane.read expected d reals"
  -- TODO, should we verify the hyperplane is non-vertical?
  readListPrec = readListPrecDefault


-- | Try to construct a Non-vertical hyperplane out of some generic hyperplane.
--
-- >>> asNonVerticalHyperPlane $ HyperPlane 10 1 2
-- Just (NonVerticalHyperPlane [10,1,2])
-- >>> asNonVerticalHyperPlane $ HyperPlane 10 1 0
-- Nothing
asNonVerticalHyperPlane :: ( HyperPlane_ hyperPlane d r
                           , Fractional r, Eq r, 1 <= d
                           )
                        => hyperPlane -> Maybe (NonVerticalHyperPlane d r)
asNonVerticalHyperPlane = asNonVerticalHyperPlane' . hyperPlaneEquation
{-# INLINE asNonVerticalHyperPlane #-}

-- | Constructs a non-vertical hyperplane from a vector
asNonVerticalHyperPlane'   :: forall d r. ( Has_ Vector_ d r, Has_ Vector_ (d+1) r, 1 <= d
                                          , Fractional r, Eq r)
                           => Vector (d+1) r -> Maybe (NonVerticalHyperPlane d r)
asNonVerticalHyperPlane' e
    | ad == 0   = Nothing
    | otherwise = Just $ NonVerticalHyperPlane $ a ^/ (-ad)
  where
    (a0 :: r, as :: Vector d r) = uncons e
    ad = as^.last
    a  = as&last .~ a0
{-# INLINE asNonVerticalHyperPlane' #-}

instance ( MkHyperPlaneConstraints d r
         , 2 <= d
         ) => HyperPlane_ (NonVerticalHyperPlane d r) d r where

instance ( MkHyperPlaneConstraints d r
         , 2 <= d
         ) => ConstructableHyperPlane_ (NonVerticalHyperPlane d r) d r where

  type HyperPlaneFromEquationConstraint (NonVerticalHyperPlane d r) d r = (Fractional r, Eq r)

  -- | pre: the last component is not zero
  --
  --
  -- >>> hyperPlaneFromEquation $ Vector3 2 1 (-1)
  -- NonVerticalHyperPlane [1,2]
  hyperPlaneFromEquation e = case asNonVerticalHyperPlane' e of
      Just h  -> h
      Nothing -> error "hyperPlaneFromEquation: Hyperplane is vertical!"
  {-# INLINE hyperPlaneFromEquation #-}

  fromPointAndNormal _ n = NonVerticalHyperPlane n
  -- see https://en.wikipedia.org/wiki/Normal_(geometry)
  --
  -- i.e. Alternatively, if the hyperplane is defined as the solution set of a single
  -- linear equation a_1 x_1 + ⋯ + a_n x_n = c , then the vector n = ( a_1 , .. , a_n ) is
  -- a normal.
  --
  -- FIXME: this seems fishy; don't we need the point?

  --


instance ( MkHyperPlaneConstraints d r, 1 + (d-1) ~ d
         , Num r
         , 2 <= d
         ) => NonVerticalHyperPlane_ (NonVerticalHyperPlane d r) d r where
  -- >>> myHyperPlane^.hyperPlaneCoefficients
  -- Vector2 1 2
  hyperPlaneCoefficients = coerced







--------------------------------------------------------------------------------
-- * Specific 3D Functions


-- | Shorthand for non-vertical hyperplanes in R^3
type Plane = NonVerticalHyperPlane 3


-- | Constructs a Plane in R^3 for the equation z = ax + by + c
pattern Plane       :: r -> r -> r -> Plane r
pattern Plane a b c = NonVerticalHyperPlane (Vector3 a b c)
{-# COMPLETE Plane #-}

-- | Shorthand for Non-vertical hyperplanes in R^3
type Plane_ plane = NonVerticalHyperPlane_ plane 3

-- | Destructs a Plane in R^3 into the equation z = ax + by + c
pattern Plane_       :: Plane_ plane r => r -> r -> r -> plane
pattern Plane_ a b c <- (view hyperPlaneCoefficients -> (Vector3 a b c))
{-# COMPLETE Plane_ #-}
