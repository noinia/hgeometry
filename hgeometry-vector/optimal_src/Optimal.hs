module Optimal
  ( module Optimal.Internal
  ) where

import qualified Boxed
import           Data.RealNumber.Rational
import qualified Double.V2
import qualified Float.V2
import qualified Int.V2
import           Optimal.Internal

--------------------------------------------------------------------------------

-- type instance VectorFamily 2 Int = V2.V2 Int

type instance VectorFamily 2 Int = Int.V2.Vec2
type instance VectorFamily 2 Double = Double.V2.Vec2
type instance VectorFamily 2 Float = Float.V2.Vec2


-- bleh, apparently I can't define this for arbitrary d here :(
-- I guess that is somewhat to be expected; since we may still want to define
-- the various boxed versions separately
type instance VectorFamily d (RealNumber n) = Boxed.Vector d (RealNumber n)
type instance VectorFamily d (Vector d' r)  = Boxed.Vector d (Vector d' r)

-- type instance VectorFamily 2 (RealNumber n) = Boxed.Vector 2 (RealNumber n)
-- type instance VectorFamily 3 (RealNumber n) = Boxed.Vector 3 (RealNumber n)
-- type instance VectorFamily 4 (RealNumber n) = Boxed.Vector 4 (RealNumber n)
-- type instance VectorFamily 5 (RealNumber n) = Boxed.Vector 5 (RealNumber n)
-- type instance VectorFamily 6 (RealNumber n) = Boxed.Vector 6 (RealNumber n)
-- type instance VectorFamily 7 (RealNumber n) = Boxed.Vector 7 (RealNumber n)
-- type instance VectorFamily 8 (RealNumber n) = Boxed.Vector 8 (RealNumber n)
-- type instance VectorFamily 9 (RealNumber n) = Boxed.Vector 9 (RealNumber n)


-- type instance VectorFamily 2 (Vector d' r) = Boxed.Vector 2 (Vector d' r)
-- type instance VectorFamily 3 (Vector d' r) = Boxed.Vector 3 (Vector d' r)
-- type instance VectorFamily 4 (Vector d' r) = Boxed.Vector 4 (Vector d' r)
-- type instance VectorFamily 5 (Vector d' r) = Boxed.Vector 5 (Vector d' r)
-- type instance VectorFamily 6 (Vector d' r) = Boxed.Vector 6 (Vector d' r)
-- type instance VectorFamily 7 (Vector d' r) = Boxed.Vector 7 (Vector d' r)
