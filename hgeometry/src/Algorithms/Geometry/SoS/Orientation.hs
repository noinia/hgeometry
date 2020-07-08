module Algorithms.Geometry.SoS.Orientation where

import Algorithms.Geometry.SoS.Determinant
import Algorithms.Geometry.SoS.Sign
import Algorithms.Geometry.SoS.Symbolic
import Control.CanAquire
import Control.Lens hiding (snoc,cons)
import Data.Geometry.Matrix
import Data.Geometry.Point.Class
import Data.Geometry.Vector
import GHC.TypeNats


-- | To support Simulation of Simplicity a point type p must support:
--
-- - Retrieving the Index of the point
-- - The dimension of p must support SoS
type SoS p = ( AsPoint p, HasIndex p i, Ord i
             , SoSD (Dimension p)
             )

-- | A dimension d has support for SoS when we can:
--
-- - sort a vector that has (d+1) entries and count the number of
-- - exchanges made generate the terms of the determinant of the
--   \(\Lambda_{d+1}\) matrix in the right order.
type SoSD d = ( SortI (d + 1)
              , ToTerms d
              )



-- | Given a query point q, and a vector of d points defining a
-- hyperplane test if q lies above or below the hyperplane.
--
--
sideTest      :: (AsAPoint p, HasIndex (p d r) i, Ord i, Num r, Ord r
                 , Arity d, Arity (d+1), HasDeterminant (d+1)
                 )
              => p d r -> Vector d (p d r) -> Sign
sideTest q ps = sideTest'' . fmap withSymbolic $ cons q ps


-- | Given an input point, transform its number type to include
-- symbolic $\varepsilon$ expressions so that we can use SoS.
withSymbolic   :: (AsAPoint p, HasIndex (p d r) i, Ord i, Arity d)
                 => p d r -> p d (Symbolic (i,Int) r)
withSymbolic p = let i = indexOf p
                 in p&vector' %~ imap (\j x -> symbolic x (i,j))



sideTest'      :: ( AsAPoint p, Num r, Ord r, Ord i, HasDeterminant (d+1), Arity d, Arity (d+1))
               => p d (Symbolic i r) -> Vector d (p d (Symbolic i r)) -> Sign
sideTest' q ps = sideTest'' $ cons q ps

sideTest'' :: ( AsAPoint p, Num r, Ord r, Ord i, HasDeterminant (d+1), Arity d, Arity (d+1))
           => Vector (d+1) (p d (Symbolic i r)) -> Sign
sideTest'' = signDet . Matrix . fmap mkLambdaRow


mkLambdaRow :: (AsAPoint p, Num r, Arity d, Arity (d+1)) => p d r -> Vector (d+1) r
mkLambdaRow = flip snoc 1 . view vector'
