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
  , isParallelTo

  , HyperPlaneFromPoints(..)
  ) where

import Control.Lens hiding (snoc, cons, unsnoc)
import Data.Type.Ord
import GHC.TypeNats
import HGeometry.Point.Class
import HGeometry.Properties
import HGeometry.Vector

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

  -- -- | Constructs the hyperplane through d points
  -- hyperPlaneTrough :: Num r => Point_ point d r => Vector d point -> hyperPlane

  -- | A hyperplane \(h) has coefficients \(a_i \in \mathbb{R}\) so that
  -- a \point \( (p_1,..,p_d) \) lies on \(h) iff:
  -- \( a_0  + \sum_i=1^d a_i*p_i = 0 \)
  --
  -- this fuction returns the vector of these coefficients \(\langle a_0,..,a_d \rangle\)
  hyperPlaneEquation :: ( Num r
                        ) => hyperPlane -> Vector (d+1) r
  default hyperPlaneEquation :: ( NonVerticalHyperPlane_ hyperPlane d r
                                , Num r
                                )
                             => hyperPlane -> Vector (d+1) r
  hyperPlaneEquation h = snoc a (-1)
    where
      a = hyperPlaneCoefficients h
  {-# INLINE hyperPlaneEquation #-}

  -- | Construct a Hyperplane from a point and a normal.
  fromPointAndNormal     :: ( Point_ point d r, Num r)
                         => point -> Vector d r -> hyperPlane
  default fromPointAndNormal :: ( Point_ point d r, Num r
                                , ConstructableHyperPlane_ hyperPlane d r
                                , Has_ Metric_ d r
                                )
                             => point -> Vector d r -> hyperPlane
  fromPointAndNormal q n = hyperPlaneFromEquation $ cons a0 n
    where
      a0 = negate $ (q^.vector) `dot` n
  {-# INLINE fromPointAndNormal #-}

  -- | Get the normal vector of the hyperlane.
  normalVector :: Num r => hyperPlane -> Vector d r
  default normalVector :: ( KnownNat d
                          , Num r
                          )
                       => HyperPlane_ hyperPlane d r => hyperPlane -> Vector d r
  normalVector = suffix . hyperPlaneEquation
  {-# INLINE normalVector #-}


  -- | Test if a point lies on a hyperplane.
  onHyperPlane     :: (Point_ point d r, Eq r, Num r) => point -> hyperPlane -> Bool
  default onHyperPlane :: ( Point_ point d r, Eq r, Num r
                          , Has_ Metric_ d r
                          , d < d+1
                          ) => point -> hyperPlane -> Bool
  q `onHyperPlane` h = a0 + (a `dot` (q^.vector)) == 0
    where
      (a,a0) = unsnoc $ hyperPlaneEquation h
  {-# INLINE onHyperPlane #-}

  -- | Test if a point lies on a hyperplane.
  onSideTest     :: (Point_ point d r, Ord r, Num r) => point -> hyperPlane -> Ordering
  default onSideTest :: ( Point_ point d r, Ord r, Num r
                        , Has_ Metric_ d r
                        , d < d+1 -- silly constraints
                        ) => point -> hyperPlane -> Ordering
  q `onSideTest` h = (a0 + (a `dot` (q^.vector))) `compare` 0
    where
      (a,a0) = unsnoc $ hyperPlaneEquation h
  {-# INLINE onSideTest #-}

class HyperPlane_ hyperPlane d r
     => ConstructableHyperPlane_ hyperPlane d r where

  -- | Given the coefficients \(a_0,..,a_d\) of the equation, i.e.
  -- so that
  --
  -- \( a_0  + \sum_i=1^d a_i*p_i = 0 \)
  --
  -- construct the hyperplane form it.
  --
  --
  hyperPlaneFromEquation :: Vector (d+1) r -> hyperPlane


--------------------------------------------------------------------------------

-- | Non-vertical hyperplanes.
class HyperPlane_ hyperPlane d r => NonVerticalHyperPlane_ hyperPlane d r where
--  {-# MINIMAL  #-}

  -- | Get the coordinate in dimesnion $d$ of the hyperplane at the given position.
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
  evalAt p h = hyperPlaneCoefficients h `dot` cons 1 (p^.vector)
  {-# INLINE evalAt #-}


  -- | The coefficients \( \langle a_1,..,a_d \rangle \) such that
  -- a point \(p = (p_1,..,p_d) \) lies on the hyperplane the given coefficients iff
  --
  --
  --  \( a_d + \sum_i=1^{d-1} a_i*p_i = p_d \)
  --
  hyperPlaneCoefficients :: hyperPlane -> Vector d r
  default hyperPlaneCoefficients :: ( Fractional r, KnownNat d
                                    , d < d+1
                                    ) => hyperPlane -> Vector d r
  hyperPlaneCoefficients h = a ^/ (-x)
    where
      (a,x) = unsnoc $ hyperPlaneEquation h
  {-# INLINE hyperPlaneCoefficients #-}


--------------------------------------------------------------------------------
-- * Functions on Hyperplanes

-- | Test if two hyperplanes are parallel.
isParallelTo       :: ( HyperPlane_ hyperPlane  d r
                      , HyperPlane_ hyperPlane' d r
                      , Has_ Additive_ d r
                      , Num r, Eq r
                      ) => hyperPlane -> hyperPlane' -> Bool
isParallelTo h1 h2 = sameDirection (normalVector h1) (normalVector h2)


class HyperPlaneFromPoints hyperPlane where
  -- | Construct a hyperplane through the given d points.
  hyperPlaneThrough     :: ( Point_ point d r
                           , HyperPlane_ hyperPlane d r
                           , Num r
                           ) => Vector d point -> hyperPlane
