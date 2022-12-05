{-# LANGUAGE DefaultSignatures #-}
{-# OPTIONS_GHC -fplugin GHC.TypeLits.KnownNat.Solver #-}
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
  , NonVerticalHyperPlane_(..)
  , isParallelTo
  ) where

import Control.Lens hiding (snoc, cons, unsnoc)
import Data.Type.Ord
import GHC.TypeNats
import HGeometry.Point.Class
import HGeometry.Properties
import HGeometry.Vector (sameDirection)
import HGeometry.Vector.Class

--------------------------------------------------------------------------------

-- | A class to represent hyperplanes in d-dimensional space.
class ( NumType hyperPlane ~ r
      , Dimension hyperPlane ~ d
      , NumType (EquationFor hyperPlane) ~ r
      , Vector_ (VectorFor hyperPlane) d r
      , Vector_ (EquationFor hyperPlane) (d+1) r
      ) => HyperPlane_ hyperPlane d r | hyperPlane -> d
                                      , hyperPlane -> r where
  type EquationFor hyperPlane

  -- | Given the equation, construct the hyperplane form it.
  hyperPlaneFromEquation :: EquationFor hyperPlane -> hyperPlane

  -- -- | Constructs the hyperplane through d points
  -- hyperPlaneTrough :: Num r => Point_ point d r => Vector d point -> hyperPlane

  -- | A hyperplane \(h) has coefficients \(a_i \in \mathbb{R}\) so that
  -- a \point \( (p_1,..,p_d) \) lies on \(h) iff:
  -- \( a_0  + \sum_i=1^d a_i*p_i = 0 \)
  --
  -- this fuction returns the vector of these coefficients \(\langle a_0,..,a_d \rangle\)
  hyperPlaneEquation :: ( Num r
                        ) => hyperPlane -> EquationFor hyperPlane
  default hyperPlaneEquation :: ( NonVerticalHyperPlane_ hyperPlane d r
                                , Num r
                                )
                             => hyperPlane -> EquationFor hyperPlane
  hyperPlaneEquation h = snoc a (-1)
    where
      a = hyperPlaneCoefficients h

  -- | Construct a Hyperplane from a point and a normal.
  fromPointAndNormal     :: ( Point_ point d r
                            , vector ~ VectorFor point
                            , Num r
                            )
                         => point -> vector -> hyperPlane
  fromPointAndNormal q n = hyperPlaneFromEquation $ cons a0 n
    where
      a0 = negate $ (q^.vector) `dot` n


  -- | Get the normal vector of the hyperlane.
  normalVector :: Num r => hyperPlane -> VectorFor hyperPlane
  default normalVector :: ( KnownNat d
                          , Num r
                          )
                       => HyperPlane_ hyperPlane d r => hyperPlane -> VectorFor hyperPlane
  normalVector = suffix . hyperPlaneEquation

  -- | Test if a point lies on a hyperplane.
  onHyperPlane     :: (Point_ point d r, Eq r, Num r) => point -> hyperPlane -> Bool
  default onHyperPlane :: ( Point_ point d r, Eq r, Num r
                          , KnownNat d
                          , d < d+1
                          ) => point -> hyperPlane -> Bool
  q `onHyperPlane` h = a0 + (a `dot` (q^.vector)) == 0
    where
      (a,a0) = unsnoc $ hyperPlaneEquation h

  -- | Test if a point lies on a hyperplane.
  onSideTest     :: (Point_ point d r, Ord r, Num r) => point -> hyperPlane -> Ordering
  default onSideTest :: ( Point_ point d r, Ord r, Num r
                        , KnownNat d
                        , d < d+1 -- silly constraints
                        ) => point -> hyperPlane -> Ordering
  q `onSideTest` h = (a0 + (a `dot` (q^.vector))) `compare` 0
    where
      (a,a0) = unsnoc $ hyperPlaneEquation h

--  {-# MINIMAL hyperPlaneTrough #-}

--------------------------------------------------------------------------------


-- | Non-vertical hyperplanes.
class HyperPlane_ hyperPlane d r => NonVerticalHyperPlane_ hyperPlane d r where

  -- | Get the coordinate in dimesnion $d$ of the hyperplane at the given position.
  evalAt     :: ( Num r
                , Metric_ (VectorFor hyperPlane)
                , 1 <= d
                ) => Point_ point (d-1) r => point -> hyperPlane -> r
  evalAt p h = hyperPlaneCoefficients h `dot` cons 1 (p^.vector)

  -- | The coefficients \( \langle a_0,..,a_{d-1} \rangle \) such that
  -- a point \(p = (p_1,..,p_d) \) lies on the hyperplane the given coefficients iff
  --
  --
  --  \( a_0 + \sum_i=1^{d-1} a_i*p_i = p_d \)
  --
  hyperPlaneCoefficients :: hyperPlane -> VectorFor hyperPlane
  default hyperPlaneCoefficients :: ( Fractional r, KnownNat d
                                    , d < d+1
                                    ) => hyperPlane -> VectorFor hyperPlane
  hyperPlaneCoefficients h = a ^/ (-x)
    where
      (a,x) = unsnoc $ hyperPlaneEquation h

  {-# MINIMAL  #-}

--------------------------------------------------------------------------------
-- * Functions on Hyperplanes

-- | Test if two hyperplanes are parallel.
isParallelTo       :: ( HyperPlane_ hyperPlane  d r
                      , HyperPlane_ hyperPlane' d r
                      , Num r, Eq r
                      ) => hyperPlane -> hyperPlane' -> Bool
isParallelTo h1 h2 = sameDirection (normalVector h1) (normalVector h2)
