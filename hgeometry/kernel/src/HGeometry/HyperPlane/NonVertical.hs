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
  , module HGeometry.HyperPlane.Class
  ) where

import           Control.DeepSeq
import           Control.Lens hiding (snoc, uncons, unsnoc)
import qualified Data.Foldable as F
import           Data.Functor.Classes
import           Data.Type.Ord
import           GHC.TypeLits
import           HGeometry.HyperPlane.Class
import           HGeometry.HyperPlane.Internal (MkHyperPlaneConstraints)
import           HGeometry.Properties
import           HGeometry.Vector
import           Prelude hiding (last)
import           Text.Read ( Read (..), Lexeme(Ident), parens, prec, step
                           , readListPrecDefault,  readListDefault
                           )
import           GHC.Show (showSpace)
import           GHC.Read (expectP)

--------------------------------------------------------------------------------

-- $setup
-- >>> import HGeometry.HyperPlane
-- >>> let myLineAsNV  = NonVerticalHyperPlane (Vector2 1 2) :: NonVerticalHyperPlane 2 Double
-- >>> let myOtherLine = HyperPlane2 4 3 2                   :: HyperPlane 2 Double
-- >>> let myPlane     = HyperPlane3 10 2 3 (-1)             :: HyperPlane 3 Double

-- | A non-vertical Hyperplane described by \( x_d = a_d + \sum_{i=1}^{d-1}
-- a_i * x_i \) where \(\langle a_1,..,a_d \rangle \) are the
-- coefficients of te hyperplane.
--
--
-- e.g. the 'myLineAsNV' defines the hyperplane (i.e. the line)
-- described by
--
-- y = 2 + 1*x
--
newtype NonVerticalHyperPlane d r = NonVerticalHyperPlane (Vector d r)

type instance NumType   (NonVerticalHyperPlane d r) = r
type instance Dimension (NonVerticalHyperPlane d r) = d

deriving newtype instance Eq     (Vector d r) => Eq     (NonVerticalHyperPlane d r)
deriving newtype instance Ord    (Vector d r) => Ord    (NonVerticalHyperPlane d r)
deriving newtype instance NFData (Vector d r) => NFData (NonVerticalHyperPlane d r)

deriving stock instance Functor     (Vector d) => Functor     (NonVerticalHyperPlane d)
deriving stock instance Foldable    (Vector d) => Foldable    (NonVerticalHyperPlane d)
deriving stock instance Traversable (Vector d) => Traversable (NonVerticalHyperPlane d)


--------------------------------------------------------------------------------


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

----------------------------------------
-- Use a more compact representation for Planes in R^3

instance {-# OVERLAPPING #-} Show r => Show (NonVerticalHyperPlane 3 r) where
  showsPrec d (Plane a b c) = showParen (d >= 11) $
    showString "Plane " . showsPrec 11 a . showSpace . showsPrec 11 b . showSpace . showsPrec 11 c

instance {-# OVERLAPPING #-} Read r => Read (NonVerticalHyperPlane 3 r) where
  readPrec = parens $ prec 10 (do expectP (Ident "Plane")
                                  a <- step readPrec
                                  b <- step readPrec
                                  c <- step readPrec
                                  pure (Plane a b c)
                              )
  readList = readListDefault
  readListPrec = readListPrecDefault


--------------------------------------------------------------------------------


-- | Try to construct a Non-vertical hyperplane out of some generic hyperplane.
--
-- >>> asNonVerticalHyperPlane $ (HyperPlane2 10 1 2 :: HyperPlane 2 Double)
-- Just (NonVerticalHyperPlane [-0.5,-5.0])
-- >>> asNonVerticalHyperPlane $ (HyperPlane2 10 1 0 :: HyperPlane 2 Double)
-- Nothing
-- >>> asNonVerticalHyperPlane myOtherLine
-- Just (NonVerticalHyperPlane [-1.5,-2.0])
-- >>> asNonVerticalHyperPlane myPlane
-- Just (Plane 2.0 3.0 10.0)
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
    | otherwise = Just $ NonVerticalHyperPlane $ a ^/ (negate ad)
  where
    (a0 :: r, as :: Vector d r) = uncons e
    ad = as^.last
    a  = as&last .~ a0
{-# INLINE asNonVerticalHyperPlane' #-}



instance ( MkHyperPlaneConstraints d r, Has_ Additive_ (d-1) r
         , 2 <= d
         ) => HyperPlane_ (NonVerticalHyperPlane d r) d r where

  -- normalVector h = let a = suffix $ hyperPlaneEquation h
  --                  in if signum (a^.last) == 1 then a else negated a

instance ( MkHyperPlaneConstraints d r, Has_ Additive_ (d-1) r
         , Fractional r, Eq r
         , 2 <= d
         ) => ConstructableHyperPlane_ (NonVerticalHyperPlane d r) d r where

  -- | pre: the last component is not zero
  --
  --
  -- >>> hyperPlaneFromEquation $ Vector3 2 1 (-1)
  -- NonVerticalHyperPlane [1,2]
  hyperPlaneFromEquation e = case asNonVerticalHyperPlane' e of
      Just h  -> h
      Nothing -> error "hyperPlaneFromEquation: Hyperplane is vertical!"
  {-# INLINE hyperPlaneFromEquation #-}


instance ( MkHyperPlaneConstraints d r, 1 + (d-1) ~ d, Has_ Additive_ (d-1) r
         , Num r
         , 2 <= d
         ) => NonVerticalHyperPlane_ (NonVerticalHyperPlane d r) d r where
  -- >>> myLineAsNV^.hyperPlaneCoefficients
  -- Vector2 1 2
  hyperPlaneCoefficients = coerced





--------------------------------------------------------------------------------
-- * Specific 2D Functions

-- -- | Constructs a Line in R^2 for the equation y = ax + b
-- pattern Line2     :: r -> r -> NonVerticalHyperPlane 2 r
-- pattern Line2 a b = NonVerticalHyperPlane (Vector2 a b)
-- {-# COMPLETE Line2 #-}

--------------------------------------------------------------------------------
-- * Specific 3D Functions


-- | Shorthand for non-vertical hyperplanes in R^3
type Plane = NonVerticalHyperPlane 3

-- | Constructs a Plane in R^3 for the equation z = ax + by + c
pattern Plane       :: r -> r -> r -> Plane r
pattern Plane a b c = NonVerticalHyperPlane (Vector3 a b c)
{-# COMPLETE Plane #-}

--------------------------------------------------------------------------------

-- | Access the last element of a vector
last :: forall vector d r. (Vector_ vector d r, 1 <= d) => IndexedLens' Int vector r
last = component @(d-1)
