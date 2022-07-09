{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Geometry.Point.Class where

import           Control.Lens
import           Data.Proxy (Proxy(..))
import           GHC.TypeNats
import           Geometry.Properties
import           Geometry.Vector
import qualified Geometry.Vector as Vector

--------------------------------------------------------------------------------

-- -- $setup
-- -- >>> import Geometry.Point.Internal (pattern Point2, pattern Point3, pattern Point4, origin)

class ( Affine (point d)
      -- , Foldable (Diff (point d))
      , Diff (point d) ~ Vector d
      , NumType (point d r) ~ r
      , Dimension (point d r) ~ d
      , Arity d
      ) => Point_ point d r where

  -- | Construct a point from a vector
  fromVector :: Vector d r -> point d r

  -- | Lens to access the vector corresponding to this point.
  --
  -- >>> (Point3 1 2 3) ^. asVector
  -- Vector3 1 2 3
  -- >>> origin & asVector .~ Vector3 1 2 3
  -- Point3 1 2 3
  asVector :: Lens (point d r) (point d s) (Vector d r) (Vector d s)


  -- | Traversal over *all* coordinates of the points
  --
  -- >>> itraverseOf coordinates (\i x -> print (i,x)) (Point2 10 20)
  -- (0,10)
  -- (1,20)
  -- Point2 () ()
  coordinates :: IndexedTraversal Int (point d r) (point d s) r s
  coordinates = asVector . reindexed (+1) itraversed
  {-# INLINE coordinates #-}

  -- | Get the coordinate in a given dimension. Consider using 'coord'
  -- instead, this is just a way of implementing that function more easily.
  --
  -- >>> Point3 1 2 3 ^. coordProxy (Proxy @2)
  -- 2
  -- >>> Point3 1 2 3 & coordProxy (Proxy @1) .~ 10
  -- Point3 10 2 3
  -- >>> Point3 1 2 3 & coordProxy (Proxy @3) %~ (+1)
  -- Point3 1 2 4
  coordProxy    :: (1 <= i, i <= d, KnownNat i)
                => proxy i -> IndexedLens' Int (point d r) r
  coordProxy px = singular $ unsafeCoord (fromIntegral . natVal $ px)
  {-# INLINE coordProxy #-}

  -- | Get the coordinate in a given dimension. This operation is unsafe in the
  -- sense that no bounds are checked. Consider using `coord` instead.
  --
  --
  -- >>> Point3 1 2 3 ^. unsafeCoord 2
  -- 2
  unsafeCoord   :: Int -> IndexedTraversal' Int (point d r) r
  unsafeCoord i = asVector . Vector.element' (i - 1)
                -- vectors are 0 indexed, whereas we are  indexed.
  {-# INLINE  unsafeCoord #-}

  {-# MINIMAL fromVector, asVector  #-}


  -- | Get the coordinate in a given dimension
  --
  -- >>> Point3 1 2 3 ^. coord @2
  -- 2
  -- >>> Point3 1 2 3 & coord @1 .~ 10
  -- Point3 10 2 3
  -- >>> Point3 1 2 3 & coord @3 %~ (+1)
  -- Point3 1 2 4
coord :: forall i point d r. (1 <= i, i <= d, KnownNat i, Point_ point d r)
      => IndexedLens' Int (point d r) r
coord = coordProxy $ Proxy @i
{-# INLINE coord #-}

--------------------------------------------------------------------------------

-- | A bidirectional pattern synonym for 1 dimensional points.
pattern Point1   :: Point_ point 1 r => r -> point 1 r
pattern Point1 x <- (view asVector -> Vector1 x)
  where
    Point1 x = fromVector (Vector1 x)

-- | A bidirectional pattern synonym for 2 dimensional points.
pattern Point2     :: Point_ point 2 r => r -> r -> point 2 r
pattern Point2 x y <- (view asVector -> Vector2 x y)
  where
    Point2 x y = fromVector (Vector2 x y)


-- | A bidirectional pattern synonym for 3 dimensional points.
pattern Point3       :: Point_ point 3 r => r -> r -> r -> point 3 r
pattern Point3 x y z <- (view asVector -> Vector3 x y z)
  where
    Point3 x y z = fromVector (Vector3 x y z)

-- | A bidirectional pattern synonym for 4 dimensional points.
pattern Point4         :: Point_ point 4 r => r -> r -> r -> r -> point 4 r
pattern Point4 x y z w <- (view asVector -> Vector4 x y z w)
  where
    Point4 x y z w = fromVector (Vector4 x y z w)


-- | Point representing the origin in d dimensions
--
-- >>> origin :: Point 4 Int
-- Point4 0 0 0 0
origin :: (Arity d, Num r, Point_ point d r) => point d r
origin = fromVector $ pure 0

--------------------------------------------------------------------------------

-- | Constructs a point from a list of coordinates. The length of the
-- list has to match the dimension exactly.
--
-- >>> pointFromList [1,2,3] :: Maybe (Point 3 Int)
-- Just (Point3 1 2 3)
-- >>> pointFromList [1] :: Maybe (Point 3 Int)
-- Nothing
-- >>> pointFromList [1,2,3,4] :: Maybe (Point 3 Int)
-- Nothing
pointFromList :: (Arity d, Point_ point d r) => [r] -> Maybe (point d r)
pointFromList = fmap fromVector . Vector.vectorFromList

-- | Project a point down into a lower dimension.
projectPoint :: ( Arity i, i <= d
                , Point_ point d r, Point_ point i r
                ) => point d r -> point i r
projectPoint = fromVector . prefix . view asVector

--------------------------------------------------------------------------------


-- | Shorthand to access the first coordinate
--
-- >>> Point3 1 2 3 ^. xCoord
-- 1
-- >>> Point2 1 2 & xCoord .~ 10
-- Point2 10 2
xCoord :: (1 <= d, Point_ point d r) => Lens' (point d r) r
xCoord = coord @1
{-# INLINABLE xCoord #-}

-- | Shorthand to access the second coordinate
--
-- >>> Point2 1 2 ^. yCoord
-- 2
-- >>> Point3 1 2 3 & yCoord %~ (+1)
-- Point3 1 3 3
yCoord :: (2 <= d, Point_ point d r) => Lens' (point d r) r
yCoord = coord @2
{-# INLINABLE yCoord #-}

-- | Shorthand to access the third coordinate
--
-- >>> Point3 1 2 3 ^. zCoord
-- 3
-- >>> Point3 1 2 3 & zCoord %~ (+1)
-- Point3 1 2 4
zCoord :: (3 <= d, Point_ point d r) => Lens' (point d r) r
zCoord = coord @3
{-# INLINABLE zCoord #-}

-- | Shorthand to access the fourth coordinate
--
-- >>> Point4 1 2 3 4 ^. wCoord
-- 4
-- >>> Point4 1 2 3 4 & wCoord %~ (+1)
-- Point4 1 2 3 5
wCoord :: (4 <= d, Point_ point d r) => Lens' (point d r) r
wCoord = coord @4
{-# INLINABLE wCoord #-}


--------------------------------------------------------------------------------

class HasPoints s t point point' where
  -- | Traversal over all points in the structure
  allPoints :: ( NumType s ~ r
               , NumType t ~ r'
               , Dimension s ~ d, Dimension t ~ d
               ) => Traversal s t (point d r) (point' d r')
