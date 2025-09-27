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
import           HGeometry.Point
import           Prelude hiding (last)

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

instance ( MkHyperPlaneConstraints d r, Has_ Additive_ (d-1) r
         , 2 <= d
         ) => HyperPlane_ (NonVerticalHyperPlane d r) d r where

instance ( MkHyperPlaneConstraints d r, 1 + (d-1) ~ d, Has_ Additive_ (d-1) r
         , Num r
         , 2 <= d
         ) => NonVerticalHyperPlane_ (NonVerticalHyperPlane d r) d r where
  -- >>> myLineAsNV^.hyperPlaneCoefficients
  -- Vector2 1 2
  hyperPlaneCoefficients = coerced

--------------------------------------------------------------------------------

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
