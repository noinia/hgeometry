module HGeometry.Matrix.Class
  ( Matrix_(..)

  ) where



import           Control.Arrow ((&&&))
import           Control.Lens hiding (cons,snoc,uncons,unsnoc)
import           Data.Kind
import qualified Data.List as List
import           Data.Maybe (fromMaybe)
import           Data.Proxy
import           Data.Type.Ord
import           GHC.TypeNats
import           HGeometry.Properties
import           HGeometry.Vector.Class
import           Prelude hiding (zipWith)


--------------------------------------------------------------------------------

-- | A matrix of n rows, each of m columns, storing values of type r.
type Matrix_ :: Type -> Nat -> Nat -> Type -> Constraint
class ( r ~ NumType matrix
      , Ixed matrix
      , IxValue matrix ~ r
      , Index matrix ~ (Int,Int) -- row, col
      ) => Matrix_ matrix n m r | matrix -> n
                                , matrix -> m
                                , matrix -> r where
  -- | Produces the Identity Matrix.
  identityMatrix :: Num r => matrix

  -- | Matrix multiplication
  multM :: ( Matrix_ matrix'  m m' r
           , Matrix_ matrix'' n m' r
           , Num r
           ) => matrix -> matrix' -> matrix''

  -- | Multiply a matrix and a vector.
  mult  :: ( Vector_ vector m r
           , Vector_ vector' n r
           , Num r) => matrix -> vector -> vector'


-- | Dimensions for which we can compute the determinant of a matrix
class HasDeterminant d where
  det :: (Num r, Matrix_ matrix d d r) => matrix -> r

-- | Class of matrices that are invertible.
class Invertible n r where
  inverse' :: (Matrix_ matrix n n r) => matrix -> matrix



-- -- | Creates a row with zeroes everywhere, except at position i, where the
-- -- value is the supplied value.
-- mkRow     :: forall vector d r. (Vector_ vector d r, Num r) => Int -> r -> vector
-- mkRow i x = set (unsafeComponent i) x zero


--------------------------------------------------------------------------------
