{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
{-# OPTIONS_GHC -fplugin-opt GHC.TypeLits.Normalise:allow-negated-numbers #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.Normalise #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.HyperPlane.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Classes for representing hyperplanes in d-dimensional space.
--
--------------------------------------------------------------------------------
module HGeometry.HyperPlane.Class
  ( HyperPlane_(..)
  , ConstructableHyperPlane_(..)
  , NonVerticalHyperPlane_(..)
  , Plane_, pattern Plane_
  , isParallelTo
  , pointOn

  , HyperPlaneFromPoints(..)
  , showPlaneEquation
  ) where

import Control.Lens hiding (snoc, cons, uncons, unsnoc)
import Data.Default
import Data.Kind (Constraint)
import Data.Type.Ord
import GHC.TypeNats
import HGeometry.Ext
import HGeometry.Point
import HGeometry.Properties
import HGeometry.Vector
import Prelude hiding (head, last)

--------------------------------------------------------------------------------

-- | myLine, myHyperPlane, and myNVHyperPlane all represent the same
-- 2d hyperplane; i.e. a line in R^2.

-- $setup
-- >>> import HGeometry.HyperPlane.NonVertical
-- >>> import HGeometry.HyperPlane
-- >>> import HGeometry.Line.LineEQ
--
-- >>> let myLine      = LineEQ 1 2                          :: LineEQ Double
-- >>> let myLineAgain = HyperPlane (Vector3 2 1 (-1))       :: HyperPlane 2 Double
-- >>> let myLineAsNV  = NonVerticalHyperPlane (Vector2 1 2) :: NonVerticalHyperPlane 2 Double
-- >>> let myVerticalLine = HyperPlane (Vector3 5 (-1) 0)    :: HyperPlane 2 Double
--
-- >>> let myOtherLine   = HyperPlane2 4 3 2                   :: HyperPlane 2 Double
-- >>> let myOtherNVLine = NonVerticalHyperPlane (Vector2 (-3/2 :: Double) (-2))
-- >>> let myPlane       = HyperPlane3 10 2 3 (-1)             :: HyperPlane 3 Double
--
--
-- 'myLine', 'myLineAgain', and 'myLineAsNV' all represent the line y = 1*x + 2
--
-- 'myVerticalLine' represents the vertical line x=5
--
--
-- 'myOtherLine' represents the line 4 + 3*x + 2*y = 0, in other words y = -(3/2)*x - 2
--
-- 'myPlane' represents the plane z = 2*x + 3*y + 10


-- | A class to represent hyperplanes in d-dimensional space.
class ( NumType hyperPlane ~ r
      , Dimension hyperPlane ~ d
      , Has_ Vector_ d r
      , Has_ Vector_ (1+d) r
      -- , NumType (EquationFor hyperPlane) ~ r
      ) => HyperPlane_ hyperPlane d r | hyperPlane -> d
                                      , hyperPlane -> r where
--  {-# MINIMAL hyperPlaneTrough #-}

  -- | Evalute the expression \(a_0 + \sum_i=1^d a_i*p_i \) at the given point.
  --
  --
  -- >>> evalHyperPlaneEquation myLine (Point2 5 10)
  -- -3.0
  evalHyperPlaneEquation     :: (Num r, Point_ point d r) => hyperPlane -> point -> r
  default evalHyperPlaneEquation :: (Num r, Point_ point d r, Has_ Metric_ (d+1) r)
                                 => hyperPlane -> point -> r
  evalHyperPlaneEquation h p = hyperPlaneEquation h `dot` (cons 1 $ p^.vector)
  {-# INLINE evalHyperPlaneEquation #-}


  -- -- | Constructs the hyperplane through d points
  -- hyperPlaneTrough :: Num r => Point_ point d r => Vector d point -> hyperPlane

  -- | A hyperplane \(h) has coefficients \(a_i \in \mathbb{R}\) so that
  -- a \point \( (p_1,..,p_d) \) lies on \(h) iff:
  -- \( a_0  + \sum_i=1^d a_i*p_i = 0 \)
  --
  -- this fuction returns the vector of these coefficients \(\langle a_0,..,a_d \rangle\).
  --
  -- >>> hyperPlaneEquation myLine
  -- Vector3 2.0 1.0 (-1.0)
  -- >>> hyperPlaneEquation myLineAgain
  -- Vector3 2.0 1.0 (-1.0)
  -- >>> hyperPlaneEquation myLineAsNV
  -- Vector3 2.0 1.0 (-1.0)
  --
  --
  -- so for example myLineAsNV actually a line with slope 1 through the point (0,2)
  --
  hyperPlaneEquation :: ( Num r
                        ) => hyperPlane -> Vector (d+1) r
  default hyperPlaneEquation :: ( NonVerticalHyperPlane_ hyperPlane d r
                                , Num r
                                , 1 <= d
                                )
                             => hyperPlane -> Vector (d+1) r
  hyperPlaneEquation h = cons a0 a
    where
      a' = h^.hyperPlaneCoefficients
      a  = a'&last .~ -1
      a0 = a'^.last
  {-# INLINE hyperPlaneEquation #-}

  -- | Get the normal vector of the hyperplane. The vector points into
  -- the positive halfspace.
  --
  -- >>> normalVector myVerticalLine
  -- Vector2 1.0 (-0.0)
  -- >>> normalVector myLine
  -- Vector2 (-1.0) 1.0
  normalVector :: (Num r, Eq r, 1 <= d) => hyperPlane -> Vector d r
  default normalVector :: (KnownNat d, Num r, Eq r, 1 <= d)
                       => hyperPlane -> Vector d r
  normalVector h = negated . suffix $ hyperPlaneEquation h
  {-# INLINE normalVector #-}
  -- https://en.wikipedia.org/wiki/Normal_(geometry)#Hypersurfaces_in_n-dimensional_space
  -- states that: if the hyperplane is defined as the solution set of a single linear equation
  -- a1*x1 + .. + an*xn = c
  -- then the vector n = [a1,.., an] is a normal.


  -- | Test if a point lies on a hyperplane.
  --
  -- >>> Point2 0 2 `onHyperPlane` myLineAgain
  -- True
  -- >>> Point2 1 3 `onHyperPlane` myLineAgain
  -- True
  -- >>> Point2 1 5 `onHyperPlane` myLineAgain
  -- False
  --
  -- >>> Point2 0 2 `onHyperPlane` myLineAsNV
  -- True
  -- >>> Point2 1 3 `onHyperPlane` myLineAsNV
  -- True
  -- >>> Point2 1 5 `onHyperPlane` myLineAsNV
  -- False
  onHyperPlane     :: (Point_ point d r, Eq r, Num r) => point -> hyperPlane -> Bool
  default onHyperPlane :: ( Point_ point d r, Eq r, Num r
                          ) => point -> hyperPlane -> Bool
  q `onHyperPlane` h = (== 0) $ evalHyperPlaneEquation h q
  {-# INLINE onHyperPlane #-}

  -- | Test if a point lies on a hyperplane. Returns the sign when evaluating the
  -- hyperplane equation.
  --
  -- >>> Point2 0 2 `onSideTest` myLineAgain
  -- EQ
  -- >>> Point2 1 3 `onSideTest` myLineAgain
  -- EQ
  -- >>> Point2 1 5 `onSideTest` myLineAgain
  -- GT
  -- >>> Point2 4 5 `onSideTest` myLineAgain
  -- LT
  -- >>> Point2 0 0 `onSideTest` HyperPlane2 1 (-1) 0
  -- LT
  --
  -- >>> Point2 1 1 `onSideTest` myVerticalLine
  -- LT
  -- >>> Point2 10 1 `onSideTest` myVerticalLine
  -- GT
  -- >>> Point2 5 20 `onSideTest` myVerticalLine
  -- EQ
  --
  -- >>> Point2 0 1 `onSideTest` myOtherLine
  -- LT
  -- >>> Point2 0 (-2) `onSideTest` myOtherLine
  -- EQ
  -- >>> Point2 1 (-3.5) `onSideTest` myOtherLine
  -- EQ
  -- >>> Point2 1 (-4) `onSideTest` myOtherLine
  -- GT
  onSideTest     :: (Point_ point d r, Ord r, Num r) => point -> hyperPlane -> Ordering
  onSideTest q h = 0 `compare` evalHyperPlaneEquation h q
  {-# INLINE onSideTest #-}

-- | Produce a point that lies on the hyperplane. No gurantees are given about which point
--
-- >>> pointOn myLine
-- Point2 (-2.0) 0.0
pointOn   :: forall hyperPlane d r.
             ( HyperPlane_ hyperPlane d r, Eq r, Fractional r, Has_ Additive_ d r
             , FoldableWithIndex Int (Vector d)
             )
          => hyperPlane -> Point d r
pointOn h = case uncons $ hyperPlaneEquation h of
              (0, _)  -> origin
              (a0, a) -> case ifind (const (/= 0)) (a :: Vector d r) of
                           Nothing     -> error "pointOn: Invalid hyperplane"
                           Just (i,ai) ->
                             (origin :: Point d r)&vector.component' i .~ ((negate a0) / ai)
  -- We are trying to find a point p so that a0 + sum_{i=1}^d ai*pi = 0,
  -- in other words, so that
  --
  --    sum_{i=1}^d ai*pi = -a0                          (1)
  --
  -- so if a0 is actually zero, we can simply choose all the pi's in Eq 1 to be zero anyway.
  -- if a0 is not zero, then there must be at least one ai that is non-zero; otherwise
  -- we would have the equation 0 = <notzero>. Hence, we find this ai. We then rewrite Eq (1)
  -- to: ai*pi + sum_{j /= i} aj*pj = -a0. Hence, we pick all the pj's to be zero, and
  -- set pi to -a0/ai.


-- | Class representing hyperplanes with methods to construct the
-- hyperplane.
class HyperPlane_ hyperPlane d r
     => ConstructableHyperPlane_ hyperPlane d r where

  -- | Additional constraints for constructing a hyperplane from its equation.
  type HyperPlaneFromEquationConstraint hyperPlane d r :: Constraint
  type HyperPlaneFromEquationConstraint hyperPlane d r = ()

  -- | Given the coefficients \(a_0,..,a_d\) of the equation, i.e.
  -- so that
  --
  -- \( a_0  + \sum_i=1^d a_i*p_i = 0 \)
  --
  -- construct the hyperplane form it.
  --
  -- >>> hyperPlaneFromEquation (Vector3 10 2 1) :: HyperPlane 2 Int -- the line 2*x + 1*y + 10 = 0
  -- HyperPlane [10,2,1]
  -- >>> hyperPlaneFromEquation $ Vector4 100 5 3 (-1)  :: HyperPlane 3 Int -- the plane 5*x + 3*y + (-1)*z + 100= 0
  -- HyperPlane [100,5,3,-1]
  --
  -- >>> myOtherLine == hyperPlaneFromEquation (Vector3 4 3 2)
  -- True
  hyperPlaneFromEquation :: HyperPlaneFromEquationConstraint hyperPlane d r
                         => Vector (d+1) r -> hyperPlane


  -- | Construct a Hyperplane from a point and a normal. The normal points into the halfplane
  -- for which the side-test is positive.
  --
  -- >>> myVerticalLine == fromPointAndNormal (Point2 5 30) (Vector2 1 0)
  -- True
  fromPointAndNormal     :: ( Point_ point d r, Num r)
                         => point -> Vector d r -> hyperPlane
  default fromPointAndNormal :: ( Point_ point d r, Num r
                                , ConstructableHyperPlane_ hyperPlane d r
                                , HyperPlaneFromEquationConstraint hyperPlane d r
                                , Has_ Metric_ d r
                                )
                             => point -> Vector d r -> hyperPlane
  fromPointAndNormal q n = hyperPlaneFromEquation $ cons a0 (negated n)
    where
      a0 = (q^.vector) `dot` n
  {-# INLINE fromPointAndNormal #-}


-- -- | returns a point that lies on the hyperpalne
-- pointOn :: hyperPlane -> Point d r


--------------------------------------------------------------------------------

-- | Non-vertical hyperplanes.
class HyperPlane_ hyperPlane d r => NonVerticalHyperPlane_ hyperPlane d r where
  {-# MINIMAL hyperPlaneCoefficients #-}

  -- | Get the coordinate in dimension \(d\) of the hyperplane at the given position.
  --
  -- >>> evalAt (Point1 1) myLineAsNV
  -- 3.0
  -- >>> evalAt (Point1 10) myLineAsNV
  -- 12.0
  -- >>> evalAt (Point1 5) <$> asNonVerticalHyperPlane myOtherLine
  -- Just (-9.5)
  evalAt     :: ( Num r
                , 1 <= d
                , Point_ point (d-1) r
                ) => point -> hyperPlane -> r
  default evalAt :: ( Num r
                    , 1 <= d
                    , Point_ point (d-1) r
                    , 1 + (d-1) ~ d -- silly silly agian :(
                    , Has_ Metric_ d r
                    ) => point -> hyperPlane -> r
  evalAt p h = (h^.hyperPlaneCoefficients) `dot` snoc (p^.vector) 1
  {-# INLINE evalAt #-}


  -- | The coefficients \( \langle a_1,..,a_d \rangle \) such that
  -- a point \(p = (p_1,..,p_d) \) lies on the hyperplane the given coefficients iff
  --
  --
  --  \( a_d + \sum_i=1^{d-1} a_i*p_i = p_d \)
  --
  -- >>> view hyperPlaneCoefficients myLineAsNV
  -- Vector2 1.0 2.0
  -- >>> view hyperPlaneCoefficients myLine
  -- Vector2 1.0 2.0
  -- >>> view hyperPlaneCoefficients $ Plane 1 2 3
  -- Vector3 1 2 3
  -- >>> asNonVerticalHyperPlane myOtherLine ^?_Just.hyperPlaneCoefficients
  -- Just (Vector2 (-1.5) (-2.0))
  hyperPlaneCoefficients :: Lens' hyperPlane (Vector d r)

  -- | Test if a point q lies above a non-vertical hyperplane h (i.e. verticalSideTest q h
  -- == GT), on the hyperplane (== EQ), or below (LT).
  --
  -- >>> Point2 0 2 `verticalSideTest` myLineAsNV
  -- EQ
  -- >>> Point2 1 3 `verticalSideTest` myLineAsNV
  -- EQ
  -- >>> Point2 1 5 `verticalSideTest` myLineAsNV
  -- GT
  -- >>> Point2 4 5 `verticalSideTest` myLineAsNV
  -- LT
  --
  -- >>> Point2 0 1 `verticalSideTest` myOtherNVLine
  -- GT
  -- >>> Point2 0 (-2) `verticalSideTest` myOtherNVLine
  -- EQ
  -- >>> Point2 1 (-3.5) `verticalSideTest` myOtherNVLine
  -- EQ
  -- >>> Point2 1 (-4) `verticalSideTest` myOtherNVLine
  -- LT
  verticalSideTest   :: (1 <= d, Point_ point d r, Ord r, Num r)
                     => point -> hyperPlane -> Ordering
  default verticalSideTest :: ( Point_ point d r
                              , Num r, Ord r
                              , 1 <= d
                              , 1 + (d-1) ~ d -- silly silly agian :(
                              , Has_ Metric_ d r
                              , Has_ Additive_ (d-1) r
                              ) => point -> hyperPlane -> Ordering
  verticalSideTest q h = (q^.dCoord) `compare` evalAt (projectPoint q :: Point (d-1) r) h

--------------------------------------------------------------------------------
-- * Helpers for (NonVertical) Planes in R^3.

-- | Shorthand for Non-vertical hyperplanes in R^3
type Plane_ plane = NonVerticalHyperPlane_ plane 3

-- | Destructs a Plane in R^3 into the equation z = ax + by + c
pattern Plane_       :: Plane_ plane r => r -> r -> r -> plane
pattern Plane_ a b c <- (view hyperPlaneCoefficients -> (Vector3 a b c))
{-# COMPLETE Plane_ #-}

--------------------------------------------------------------------------------
-- * Functions on Hyperplanes

-- | Test if two hyperplanes are parallel.
isParallelTo       :: ( HyperPlane_ hyperPlane  d r
                      , HyperPlane_ hyperPlane' d r
                      , Has_ Metric_ d r
                      , Num r, Eq r, 1 <= d
                      ) => hyperPlane -> hyperPlane' -> Bool
isParallelTo h1 h2 = isScalarMultipleOf (normalVector h1) (normalVector h2)

-- | Class that tells us we can construct a hyperplane from a vector
-- of points.
class HyperPlaneFromPoints hyperPlane where
  -- | Construct a hyperplane through the given d points.
  hyperPlaneThrough     :: ( Point_ point d r
                           , HyperPlane_ hyperPlane d r
                           , Num r
                           ) => Vector d point -> hyperPlane


--------------------------------------------------------------------------------
-- * Functions on Non-vertical Hyperplanes

-- | Renders the defining equation of a plane in R^3 in some human readable format.
--
-- >>> showPlaneEquation (Plane 5 3 2)
-- "z = 5x + 3y + 2"
showPlaneEquation   :: (Plane_ plane r, Show r) => plane -> String
showPlaneEquation h = let (Vector3 a b c) = show <$> h^.hyperPlaneCoefficients
                      in "z = " <> a <> "x + " <> b <> "y + " <> c

--------------------------------------------------------------------------------
-- * Some Instances

instance (HyperPlane_ hyperPlane d r)
         => HyperPlane_ (hyperPlane :+ extra) d r where
  evalHyperPlaneEquation h = evalHyperPlaneEquation (h^.core)
  {-# INLINE evalHyperPlaneEquation #-}
  hyperPlaneEquation = hyperPlaneEquation . view core
  {-# INLINE hyperPlaneEquation #-}
  normalVector = normalVector . view core
  {-# INLINE normalVector #-}
  onHyperPlane q = onHyperPlane q . view core
  {-# INLINE onHyperPlane #-}
  onSideTest q = onSideTest q . view core
  {-# INLINE onSideTest #-}

instance (ConstructableHyperPlane_ hyperPlane d r, Default extra)
          => ConstructableHyperPlane_ (hyperPlane :+ extra) d r where
  type HyperPlaneFromEquationConstraint (hyperPlane :+ extra) d r =
         HyperPlaneFromEquationConstraint hyperPlane d r
  hyperPlaneFromEquation v = hyperPlaneFromEquation v :+ def
  {-# INLINE hyperPlaneFromEquation #-}
  fromPointAndNormal p v = fromPointAndNormal p v :+ def
  {-# INLINE fromPointAndNormal #-}

instance (NonVerticalHyperPlane_  hyperPlane d r)
         => NonVerticalHyperPlane_ (hyperPlane :+ extra) d r where
  evalAt p = evalAt p . view core
  {-# INLINE evalAt #-}
  hyperPlaneCoefficients = core.hyperPlaneCoefficients
  {-# INLINE hyperPlaneCoefficients #-}
  verticalSideTest q = verticalSideTest q . view core
  {-# INLINE verticalSideTest #-}


-- | Access the last element of a vector
last :: forall vector d r. (Vector_ vector d r, 1 <= d) => IndexedLens' Int vector r
last = component @(d-1)
