{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
module HGeometry.HyperPlane.Internal
  ( HyperPlane(..)
  , MkHyperPlaneConstraints
  , cmpInDirection
  ) where

import Control.Lens hiding (cons)
import Data.Type.Ord
import GHC.TypeNats
import HGeometry.HyperPlane.Class
-- import HGeometry.Line.Class
import HGeometry.Point.Class
import HGeometry.Properties
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | A Hyperplane in d-dimensions, described by
--
-- a \point \( (p_1,..,p_d) \) lies on \(h) iff:
-- \( a_0  + \sum_i=1^d a_i*p_i = 0 \)
newtype HyperPlane d r = HyperPlane (Vector (d+1) r)

type instance NumType   (HyperPlane d r) = r
type instance Dimension (HyperPlane d r) = d

--------------------------------------------------------------------------------


--------------------------------------------------------------------------------

-- | Constraints on d needed to be able to construct hyperplanes; pretty much all of
-- these are satisfied by default, it is just that the typechecker does not realize that.
type MkHyperPlaneConstraints d r =
  ( d < d+1, KnownNat d, Has_ Metric_ d r, Has_ Vector_ d r, Has_ Vector_ (d+1) r
  )

instance ( MkHyperPlaneConstraints d r
         ) => HyperPlane_ (HyperPlane d r) d r where
  hyperPlaneEquation (HyperPlane v) = v

instance ( MkHyperPlaneConstraints d r
         ) => ConstructableHyperPlane_ (HyperPlane d r) d r where
  hyperPlaneFromEquation = HyperPlane

instance ( Eq r
         ) => HyperPlaneFromPoints (HyperPlane 2 r) where
  hyperPlaneThrough (Vector2 (Point2_ px py) (Point2_ qx qy))
    | px /= qx  = let a = qy - py
                      b = px - qx
                      c = (qx-px)*py - px*(qy-py)
                  in HyperPlane $ Vector3 c a b
    | otherwise = HyperPlane $ Vector3 px (-1) 0


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
