{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Geometry.SoS.Orientation
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Orientation/side tests tests using Simulation of Simplicity.
--
--------------------------------------------------------------------------------
module Geometry.SoS.Orientation
  ( SoS
  , sideTest
  , sideTest'
  ) where

import Control.Lens hiding (snoc,cons)
import Data.Indexed
import Data.RealNumber.Symbolic
import Data.Sign
import GHC.TypeNats
import Geometry.Matrix
import Geometry.Point.Class
import Geometry.SoS.Determinant
import Geometry.SoS.Point
import Geometry.Vector

--------------------------------------------------------------------------------

-- $setup
-- >>> let with p i = WithIndex i p


-- | A dimension d has support for SoS when we can: compute a
-- dterminant of a d+1 by d+1 dimensional matrix.
type SoS d = (Arity d, HasDeterminant (d+1))

-- sideTest (Point2 1 1 `with` 0) $ Vector2 (Point2 0 0 `with` 1) (Point2 2 2 `with` 3)'



-- | Given a query point q, and a vector of d points defining a
-- hyperplane test if q lies above or below the hyperplane. Each point
-- is assumed to have an unique index of type i that can be used to
-- disambiguate it in case of degeneracies.
--
-- some 1D examples:
--
-- >>> sideTest (Point1 0 `with` 0) (Vector1 $ Point1 2 `with` 1)
-- Negative
-- >>> sideTest (Point1 10 `with` 0) (Vector1 $ Point1 2 `with` 1)
-- Positive
-- >>> sideTest (Point1 2 `with` 0) (Vector1 $ Point1 2 `with` 1)
-- Positive
-- >>> sideTest (Point1 2 `with` 3) (Vector1 $ Point1 2 `with` 1)
-- Negative
--
-- some 2D examples:
--
-- >>> sideTest (Point2 1 2 `with` 0) $ Vector2 (Point2 0 0 `with` 1) (Point2 2 2 `with` 3)
-- Positive
-- >>> sideTest (Point2 1 (-2) `with` 0) $ Vector2 (Point2 0 0 `with` 1) (Point2 2 2 `with` 3)
-- Negative
-- >>> sideTest (Point2 1 1 `with` 0) $ Vector2 (Point2 0 0 `with` 1) (Point2 2 2 `with` 3)
-- Negative
-- >>> sideTest (Point2 1 1 `with` 10) $ Vector2 (Point2 0 0 `with` 1) (Point2 2 2 `with` 3)
-- Positive
-- >>> sideTest (Point2 1 1 `with` 10) $ Vector2 (Point2 0 0 `with` 3) (Point2 2 2 `with` 1)
-- Positive
sideTest      :: ( SoS d, Num r, Ord r
                 , Point_ point d (Symbolic SoSI r)
                 , Point_ point d r
                 , HasIndex (point d r), Functor (point d))
              => point d r -> Vector d (point d r) -> Sign
sideTest q ps = sideTest'' . fmap toSymbolic $ cons q ps

-- | Given a point q and a vector of d points defining a hyperplane,
-- test on which side of the hyperplane q lies.
--
-- TODO: Specify what the sign means
sideTest'      :: (Num r, Ord r, Ord i, SoS d, Arity (d+1), Point_ point d (Symbolic i r))
               => point d (Symbolic i r) -> Vector d (point d (Symbolic i r)) -> Sign
sideTest' q ps = sideTest'' $ cons q ps

-- | Given a vector of points, tests if the point encoded in the first
-- row is above/below the hyperplane defined by the remaining points
-- (rows).
sideTest'' :: ( Num r, Ord r, Ord i, HasDeterminant (d+1)
              , Point_ point d (Symbolic i r), Arity (d+1))
           => Vector (d+1) (point d (Symbolic i r)) -> Sign
sideTest'' = signDet . Matrix . fmap mkLambdaRow

-- | Given a point produces the vector/row corresponding to this point
-- in a homogeneous matrix represetnation. I.e. we add a 1 as an
-- additonal column at the end.
mkLambdaRow :: (Num r, Point_ point d r, Arity (d+1)) => point d r -> Vector (d+1) r
mkLambdaRow = flip snoc 1 . view asVector
