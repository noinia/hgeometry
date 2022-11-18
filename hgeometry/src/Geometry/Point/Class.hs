{-# LANGUAGE  AllowAmbiguousTypes  #-}
{-# LANGUAGE  UndecidableInstances  #-}
{-# LANGUAGE  FunctionalDependencies  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Geometry.Point.Class where

import           Control.Lens
import           Data.Ext
import           HGeometry.Indexed
import           GHC.TypeNats
import           Geometry.Point.Internal (Point)
import qualified Geometry.Point.Internal as Internal
import           Geometry.Vector
import           Linear.V2
import           Linear.V3
import           Linear.V4
--------------------------------------------------------------------------------

-- $setup
-- >>> import Geometry.Point.Internal (pattern Point2, pattern Point3, pattern Point4, origin)

-- | Types that can act as d-dimensional points whose numeric type is
-- r.
--
-- Note that updates to the 'point' are not necessarily supported. See
-- ~AsAPoint~ for updatable points.
class ToAPoint point d r | point -> r where
  -- | Getter to get a 'Point d r' out of the point.
  toPoint   :: Getter point (Point d r)

-- | Types that can act a points
class AsAPoint point where
  -- | Lens to access the Point in the point. Updates may change the
  -- dimension and/or numeric type.
  asAPoint :: Lens (point d r) (point d' r') (Point d r) (Point d' r')

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

instance ToAPoint (Point d r) d r where
  toPoint = to id
  {-# INLINABLE toPoint #-}

instance ToAPoint (Point d r :+ p) d r where
  toPoint = core
  {-# INLINABLE toPoint #-}

instance AsAPoint Point where
  asAPoint = id
  {-# INLINABLE asAPoint #-}

instance ToAPoint point d r => ToAPoint (WithIndex point) d r where
  toPoint = to (\(WithIndex _ p) -> p^.toPoint)
  {-# INLINABLE toPoint #-}


instance (1 <= d, Arity d) => R1 (Point d) where
  _x = coord @1
  {-# INLINABLE _x #-}

instance (2 <= d, Arity d, 1 <= d) => R2 (Point d) where
  _y  = coord @2
  {-# INLINABLE _y #-}
  _xy = lens (\p -> V2 (p^._x) (p^._y)) (\p (V2 x y) -> p&_x .~ x
                                                         &_y .~ y

                                        )
  {-# INLINABLE _xy #-}

instance (3 <= d, Arity d, 1 <= d, 2 <= d) => R3 (Point d) where
  _z = coord @3
  {-# INLINABLE _z #-}
  _xyz = lens (\p -> V3 (p^._x) (p^._y) (p^._z))
              (\p (V3 x y z) -> p&_x .~ x
                                 &_y .~ y
                                 &_z .~ z
              )
  {-# INLINABLE _xyz #-}

instance (4 <= d, Arity d, 1 <= d, 2 <= d, 3 <= d) => R4 (Point d) where
  _w = coord @4
  {-# INLINABLE _w #-}
  _xyzw = lens (\p -> V4 (p^._x) (p^._y) (p^._z) (p^._w))
               (\p (V4 x y z w) -> p&_x .~ x
                                    &_y .~ y
                                    &_z .~ z
                                    &_w .~ w
               )
  {-# INLINABLE _xyzw #-}

-- | Shorthand to access the first coordinate
--
-- >>> Point3 1 2 3 ^. xCoord
-- 1
-- >>> Point2 1 2 & xCoord .~ 10
-- Point2 10 2
xCoord :: (1 <= d, Arity d, AsAPoint point) => Lens' (point d r) r
xCoord = coord @1
{-# INLINABLE xCoord #-}

-- | Shorthand to access the second coordinate
--
-- >>> Point2 1 2 ^. yCoord
-- 2
-- >>> Point3 1 2 3 & yCoord %~ (+1)
-- Point3 1 3 3
yCoord :: (2 <= d, Arity d, AsAPoint point) => Lens' (point d r) r
yCoord = coord @2
{-# INLINABLE yCoord #-}

-- | Shorthand to access the third coordinate
--
-- >>> Point3 1 2 3 ^. zCoord
-- 3
-- >>> Point3 1 2 3 & zCoord %~ (+1)
-- Point3 1 2 4
zCoord :: (3 <= d, Arity d, AsAPoint point) => Lens' (point d r) r
zCoord = coord @3
{-# INLINABLE zCoord #-}

-- | Shorthand to access the fourth coordinate
--
-- >>> Point4 1 2 3 4 ^. wCoord
-- 4
-- >>> Point4 1 2 3 4 & wCoord %~ (+1)
-- Point4 1 2 3 5
wCoord :: (4 <= d, Arity d, AsAPoint point) => Lens' (point d r) r
wCoord = coord @4
{-# INLINABLE wCoord #-}
