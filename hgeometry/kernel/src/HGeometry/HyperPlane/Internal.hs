{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.HyperPlane.Internal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Canonical implementation for Hyperplanes as a vector of coefficients.
--
--------------------------------------------------------------------------------
module HGeometry.HyperPlane.Internal
  ( HyperPlane(..,HyperPlane2, HyperPlane3)
  , MkHyperPlaneConstraints
  , cmpInDirection
  ) where

import           Control.Lens hiding (cons)
import qualified Data.Foldable as F
import           Data.Functor.Classes
import           Data.Type.Ord
import           GHC.TypeNats
import           HGeometry.HyperPlane.Class
import           HGeometry.Point.Class
import           HGeometry.Properties
import           HGeometry.Vector
import           Text.Read (Read (..), readListPrecDefault)

--------------------------------------------------------------------------------

-- 2 dimensional hyperplane representing the line: 2 + 1*x + (-1)* y = 0,
-- in other words, the line y = 1*x + 2

-- $setup
-- >>> let myHyperPlane2 = HyperPlane $ Vector3 2 1 (-1)

-- | A Hyperplane h in d-dimensions, described by a vector of
-- coefficients (a_0,..,a_d).
--
-- a \point \( (p_1,..,p_d) \) lies on \(h) iff:
-- \( a_0  + \sum_i=1^d a_i*p_i = 0 \)
newtype HyperPlane d r = HyperPlane (Vector (d+1) r)

-- | Construct a Hyperplane, i.e. a line in R^2
--
-- HyperPlane2 c a b represents the line ax + by + c = 0
pattern HyperPlane2       :: r -> r -> r -> HyperPlane 2 r
pattern HyperPlane2 c a b = HyperPlane (Vector3 c a b)
{-# COMPLETE HyperPlane2 #-}

-- | Construct a plane in R^3
--
-- HyperPlane3 d a b c represnest the plane ax + by + cz + d = 0
pattern HyperPlane3         :: r -> r -> r -> r -> HyperPlane 3 r
pattern HyperPlane3 d a b c = HyperPlane (Vector4 d a b c)
{-# COMPLETE HyperPlane3 #-}

type instance NumType   (HyperPlane d r) = r
type instance Dimension (HyperPlane d r) = d


deriving newtype instance Eq (Vector (d+1) r) => Eq (HyperPlane d r)

instance (Show r, Foldable (Vector (d+1))) => Show (HyperPlane d r) where
  showsPrec k (HyperPlane v) = showParen (k > app_prec) $
                               showString "HyperPlane " .
                               showsPrec 11 (F.toList v)
    where
      app_prec = 10

instance ( Read r, Has_ Vector_ (d+1) r) => Read (HyperPlane d r) where
  readPrec = readData $ readUnaryWith parseVec "HyperPlane" HyperPlane
    where
      parseVec = do lst <- readPrec
                    case vectorFromList @(Vector (d+1) r) lst of
                      Just v -> pure v
                      _      -> fail "HyperPlane.read expected d+1 reals"
  readListPrec = readListPrecDefault

--------------------------------------------------------------------------------

-- | Constraints on d needed to be able to construct hyperplanes; pretty much all of
-- these are satisfied by default, it is just that the typechecker does not realize that.
type MkHyperPlaneConstraints d r =
  ( d < d+1, KnownNat d
  , Has_ Metric_ d r, Has_ Metric_ (d+1) r
  , Has_ Vector_ d r, Has_ Vector_ (d+1) r
  )

instance ( MkHyperPlaneConstraints d r
         ) => HyperPlane_ (HyperPlane d r) d r where
  -- >>> hyperPlaneEquation myHyperPlane2
  -- Vector3 2 1 (-1)
  hyperPlaneEquation (HyperPlane v) = v

instance ( MkHyperPlaneConstraints d r
         ) => ConstructableHyperPlane_ (HyperPlane d r) d r where
  hyperPlaneFromEquation = HyperPlane

instance ( Eq r
         ) => HyperPlaneFromPoints (HyperPlane 2 r) where
  --
  --
  hyperPlaneThrough (Vector2 (Point2_ px py) (Point2_ qx qy))
    | px /= qx  = let a = qy - py
                      b = px - qx
                      c = (qx-px)*py - px*(qy-py)
                  in HyperPlane $ Vector3 c a b
    | otherwise = HyperPlane $ Vector3 px (-1) 0

instance (Num r) => HyperPlaneFromPoints (HyperPlane 3 r) where
  hyperPlaneThrough (Vector3 p q r) = let u = q .-. p
                                          v = r .-. p
                                      in fromPointAndNormal p (u `cross` v)


--  hyperPlaneTrough pts = fromPointAndNormal p0 n
--    where
--      p0 = pts^.component @0
--      -- (p0, pts') = uncons pts
--      -- vecs = (.-. p0) <$> pts'
--      n = error "hyperPlaneTrhough: undefined!"


-- | Compare points with respect to the direction given by the
-- vector, i.e. by taking planes whose normal is the given vector.

-- >>> cmpInDirection (Vector2 1 0) (Point2 5 0) (Point2 10 (0 :: Int))
-- LT
-- >>> cmpInDirection (Vector2 1 1) (Point2 5 0) (Point2 10 (0 :: Int))
-- LT
-- >>> cmpInDirection (Vector2 1 1) (Point2 5 0) (Point2 10 (10 :: Int))
-- LT
-- >>> cmpInDirection (Vector2 1 1) (Point2 15 15) (Point2 10 (10 :: Int))
-- GT
-- >>> cmpInDirection (Vector2 1 0) (Point2 15 15) (Point2 15 (10 :: Int))
-- EQ
cmpInDirection       :: forall point d r.
                        ( Ord r, Num r
                        , Has_ Metric_ (d+1) r
                        , Has_ Metric_ d r
                        , Point_ point d r
                        , d < d+1--, 0 < d
                        )
                     => Vector d r -> point -> point -> Ordering
cmpInDirection n p q = p `onSideTest` fromPointAndNormal' q n
  where
    fromPointAndNormal' q' n' = HyperPlane $ cons a0 n'
      where
        a0 = negate $ (q'^.vector) `dot` n'
