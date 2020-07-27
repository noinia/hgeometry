module Algorithms.Geometry.SoS.Orientation( SoS

                                          , sideTest
                                          , sideTest'

                                          , toSymbolic
                                          ) where

import Algorithms.Geometry.SoS.Determinant
import Algorithms.Geometry.SoS.Sign
import Algorithms.Geometry.SoS.Symbolic
import Control.Lens hiding (snoc,cons)
import Data.Ext
import Data.Geometry.Matrix
import Data.Geometry.Point
import Data.Geometry.Vector
import GHC.TypeNats

--------------------------------------------------------------------------------



-- | A dimension d has support for SoS when we can: compute a
-- dterminant of a d+1 by d+1 dimensional matrix.
type SoS d = (Arity d, HasDeterminant (d+1))

-- | Given a query point q, and a vector of d points defining a
-- hyperplane test if q lies above or below the hyperplane. Each point
-- is assumed to have an unique index of type i that can be used to
-- disambiguate it in case of degeneracies.
--
-- some 1D examples:
--
-- >>> sideTest (Point1 0 :+ 0) (Vector1 $ Point1 2 :+ 1)
-- Negative
-- >>> sideTest (Point1 10 :+ 0) (Vector1 $ Point1 2 :+ 1)
-- Positive
-- >>> sideTest (Point1 2 :+ 0) (Vector1 $ Point1 2 :+ 1)
-- Positive
-- >>> sideTest (Point1 2 :+ 3) (Vector1 $ Point1 2 :+ 1)
-- Negative
--
-- some 2D examples:
--
-- >>> sideTest (Point2 1 2 :+ 0) $ Vector2 (Point2 0 0 :+ 1) (Point2 2 2 :+ 3)
-- Positive
-- >>> sideTest (Point2 1 (-2) :+ 0) $ Vector2 (Point2 0 0 :+ 1) (Point2 2 2 :+ 3)
-- Negative
-- >>> sideTest (Point2 1 1 :+ 0) $ Vector2 (Point2 0 0 :+ 1) (Point2 2 2 :+ 3)
-- Positive
-- >>> sideTest (Point2 1 1 :+ 10) $ Vector2 (Point2 0 0 :+ 1) (Point2 2 2 :+ 3)
-- Negative
-- >>> sideTest (Point2 1 1 :+ 10) $ Vector2 (Point2 0 0 :+ 3) (Point2 2 2 :+ 1)
-- Negative
sideTest      :: (SoS d, Num r, Ord r, Ord i)
              => Point d r :+ i -> Vector d (Point d r :+ i) -> Sign
sideTest q ps = sideTest'' . fmap toSymbolic $ cons q ps

-- | Given an input point, transform its number type to include
-- symbolic $\varepsilon$ expressions so that we can use SoS.
toSymbolic          :: (Ord i, Arity d) => Point d r :+ i -> Point d (Symbolic (i,Int) r)
toSymbolic (p :+ i) = p&vector' %~ imap (\j x -> symbolic x (i,j))

-- | Given a point q and a vector of d points defining a hyperplane,
-- test on which side of the hyperplane q lies.
--
-- TODO: Specify what the sign means
sideTest'      :: (Num r, Ord r, Ord i, HasDeterminant (d+1), Arity d, Arity (d+1))
               => Point d (Symbolic i r) -> Vector d (Point d (Symbolic i r)) -> Sign
sideTest' q ps = sideTest'' $ cons q ps

-- | Given a vector of points, tests if the point encoded in the first
-- row is above/below the hyperplane defined by the remaining points
-- (rows).
sideTest'' :: (Num r, Ord r, Ord i, HasDeterminant (d+1), Arity d, Arity (d+1))
           => Vector (d+1) (Point d (Symbolic i r)) -> Sign
sideTest'' = signDet . Matrix . fmap mkLambdaRow

-- | Given a point produces the vector/row corresponding to this point
-- in a homogeneous matrix represetnation. I.e. we add a 1 as an
-- additonal column at the end.
mkLambdaRow :: (Num r, Arity d, Arity (d+1)) => Point d r -> Vector (d+1) r
mkLambdaRow = flip snoc 1 . view vector'
