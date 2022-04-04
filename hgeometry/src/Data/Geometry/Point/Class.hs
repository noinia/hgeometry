{-# LANGUAGE  AllowAmbiguousTypes  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Data.Geometry.Point.Class where

import           Control.Lens
import           Data.Geometry.Point.Internal (Point)
import qualified Data.Geometry.Point.Internal as Internal
import           Data.Geometry.Vector
import           GHC.TypeNats
import           Linear.V2
import           Linear.V3
import           Linear.V4
--------------------------------------------------------------------------------

-- $setup
-- >>> import Data.Geometry.Point.Internal (pattern Point2, pattern Point3, origin)

class ToAPoint point d r where
  toPoint   :: Getter (point d r) (Point d r)

class AsAPoint p where
  asAPoint :: Lens (p d r) (p d' r') (Point d r) (Point d' r')

-- | Lens to access the vector corresponding to this point.
--
-- >>> (Point3 1 2 3) ^. vector'
-- Vector3 1 2 3
-- >>> origin & vector' .~ Vector3 1 2 3
-- Point3 1 2 3
vector' :: AsAPoint p => Lens (p d r) (p d r') (Vector d r) (Vector d r')
vector' = asAPoint . lens Internal.toVec (const Internal.Point)

-- | Get the coordinate in a given dimension
--
-- >>> Point3 1 2 3 ^. coord @2
-- 2
-- >>> Point3 1 2 3 & coord @1 .~ 10
-- Point3 10 2 3
-- >>> Point3 1 2 3 & coord @3 %~ (+1)
-- Point3 1 2 4
coord :: forall i p d r. (1 <= i, i <= d, KnownNat i, Arity d, AsAPoint p) => Lens' (p d r) r
coord = asAPoint.Internal.coord @i

-- | Get the coordinate in a given dimension. This operation is unsafe in the
-- sense that no bounds are checked. Consider using `coord` instead.
--
--
-- >>> Point3 1 2 3 ^. unsafeCoord 2
-- 2
unsafeCoord   :: (Arity d, AsAPoint p) => Int -> Lens' (p d r) r
unsafeCoord i = asAPoint.Internal.unsafeCoord i

instance ToAPoint Point d r where
  toPoint = to id

instance AsAPoint Point where
  asAPoint = id

instance (1 <= d, Arity d) => R1 (Point d) where
  _x = xCoord
instance (2 <= d, Arity d, 1 <= d) => R2 (Point d) where
  _y  = yCoord
  _xy = lens (\p -> V2 (p^.xCoord) (p^.yCoord)) (\p (V2 x y) -> p&xCoord .~ x
                                                                 &yCoord .~ y
                                                )
instance (3 <= d, Arity d, 1 <= d, 2 <= d) => R3 (Point d) where
  _z = zCoord
  _xyz = lens (\p -> V3 (p^.xCoord) (p^.yCoord) (p^.zCoord))
              (\p (V3 x y z) -> p&xCoord .~ x
                                 &yCoord .~ y
                                 &zCoord .~ z
              )
instance (4 <= d, Arity d, 1 <= d, 2 <= d, 3 <= d) => R4 (Point d) where
  _w = zCoord
  _xyzw = lens (\p -> V4 (p^.xCoord) (p^.yCoord) (p^.zCoord) (p^.coord @4))
               (\p (V4 x y z w) -> p&xCoord   .~ x
                                    &yCoord   .~ y
                                    &zCoord   .~ z
                                    &coord @4 .~ w
               )

-- | Shorthand to access the first coordinate 1
--
-- >>> Point3 1 2 3 ^. xCoord
-- 1
-- >>> Point2 1 2 & xCoord .~ 10
-- Point2 10 2
xCoord :: (1 <= d, Arity d, AsAPoint point) => Lens' (point d r) r
xCoord = coord @1
{-# INLINABLE xCoord #-}

-- | Shorthand to access the second coordinate 2
--
-- >>> Point2 1 2 ^. yCoord
-- 2
-- >>> Point3 1 2 3 & yCoord %~ (+1)
-- Point3 1 3 3
yCoord :: (2 <= d, Arity d, AsAPoint point) => Lens' (point d r) r
yCoord = coord @2
{-# INLINABLE yCoord #-}

-- | Shorthand to access the third coordinate 3
--
-- >>> Point3 1 2 3 ^. zCoord
-- 3
-- >>> Point3 1 2 3 & zCoord %~ (+1)
-- Point3 1 2 4
zCoord :: (3 <= d, Arity d, AsAPoint point) => Lens' (point d r) r
zCoord = coord @3
{-# INLINABLE zCoord #-}

-- | Shorthand to access the third coordinate 4
--
-- >>> Point4 1 2 3 4 ^. wCoord
-- 3
-- >>> Point4 1 2 3 4 & wCoord %~ (+1)
-- Point3 1 2 3 5
wCoord :: (4 <= d, Arity d, AsAPoint point) => Lens' (point d r) r
wCoord = coord @4
{-# INLINABLE wCoord #-}
