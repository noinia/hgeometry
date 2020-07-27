module Data.Geometry.Point.Class where

import           Control.Lens
import           Data.Geometry.Point.Internal (Point)
import qualified Data.Geometry.Point.Internal as Internal
import           Data.Geometry.Vector
import           GHC.TypeNats

--------------------------------------------------------------------------------

-- $setup
-- >>> import Data.Geometry.Point.Internal (pattern Point2, pattern Point3)

class ToAPoint point d r where
  toPoint   :: Prism' (point d r) (Point d r)

class AsAPoint p where
  asAPoint :: Lens (p d r) (p d' r') (Point d r) (Point d' r')

vector' :: AsAPoint p => Lens (p d r) (p d r') (Vector d r) (Vector d r')
vector' = asAPoint . lens Internal.toVec (const Internal.Point)

coord   :: (1 <= i, i <= d, KnownNat i, Arity d, AsAPoint p) => proxy i -> Lens' (p d r) r
coord i = asAPoint.Internal.coord i

unsafeCoord   :: (Arity d, AsAPoint p) => Int -> Lens' (p d r) r
unsafeCoord i = asAPoint.Internal.unsafeCoord i

instance ToAPoint Point d r where
  toPoint = prism' id Just

instance AsAPoint Point where
  asAPoint = id




-- | Shorthand to access the first coordinate C 1
--
-- >>> Point3 1 2 3 ^. xCoord
-- 1
-- >>> Point2 1 2 & xCoord .~ 10
-- Point2 [10,2]
xCoord :: (1 <= d, Arity d, AsAPoint point) => Lens' (point d r) r
xCoord = coord (C :: C 1)
{-# INLINABLE xCoord #-}

-- | Shorthand to access the second coordinate C 2
--
-- >>> Point2 1 2 ^. yCoord
-- 2
-- >>> Point3 1 2 3 & yCoord %~ (+1)
-- Point3 [1,3,3]
yCoord :: (2 <= d, Arity d, AsAPoint point) => Lens' (point d r) r
yCoord = coord (C :: C 2)
{-# INLINABLE yCoord #-}

-- | Shorthand to access the third coordinate C 3
--
-- >>> Point3 1 2 3 ^. zCoord
-- 3
-- >>> Point3 1 2 3 & zCoord %~ (+1)
-- Point3 [1,2,4]
zCoord :: (3 <= d, Arity d,AsAPoint point) => Lens' (point d r) r
zCoord = coord (C :: C 3)
{-# INLINABLE zCoord #-}
