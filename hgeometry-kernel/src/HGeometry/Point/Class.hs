{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HGeometry.Point.Class
  ( HasVector(..)
  , Affine_(..)
  , Point_(..), pattern Point1_, pattern Point2_, pattern Point3_, pattern Point4_
  , origin
  , pointFromPoint, pointFromList
  , coord, xCoord, yCoord, zCoord, wCoord

  , projectPoint
  , PointFor
  , HasPoints(..), HasPoints'
  ) where

import           Control.Lens
import           Data.Ext
import           Data.Function (on)
import           Data.Proxy (Proxy(..))
import           GHC.TypeNats
import           HGeometry.Properties
import qualified HGeometry.Vector as Vector
import           HGeometry.Vector.Class

--------------------------------------------------------------------------------

-- $setup
-- >>> import HGeometry.Point
-- >>> import HGeometry.Vector
-- >>> let myPoint = Point3 1 2 3 :: Point 3 Int

class ( NumType point ~ r
      , NumType point' ~ s
      , Vector_ (VectorFor point) (Dimension point) r
      ) => HasVector point point' r s where

  -- | Lens to access the vector corresponding to this point.
  --
  -- >>> myPoint ^. vector
  -- Vector3 1 2 3
  -- >>> myPoint & vector .~ Vector3 3 2 1
  -- Point3 3 2 1
  vector :: ( Dimension point ~ d, Dimension point' ~ d)
         => Lens point point' (VectorFor point) (VectorFor point')

-- | Affine space; essentially the same as Linear.Affine, but for
-- points of kind Type rather than (Type -> Type).
class ( Vector_ (VectorFor point) (Dimension point) (NumType point)
      , Metric_ (VectorFor point)
      ) => Affine_ point where

  -- | p .-. q represents the vector from q to p
  (.-.) :: Num (NumType point) => point -> point -> VectorFor point

  -- | add a vector to a point
  (.+^) :: Num (NumType point) => point -> VectorFor point -> point

  -- | subtract a vector from a point
  (.-^) :: Num (NumType point) => point -> VectorFor point -> point
  p .-^ v = p .+^ negated v
  {-# MINIMAL (.-.), (.+^) #-}

class ( Dimension point ~ d
      , NumType point   ~ r
      , HasVector point point r r
      , Affine_ point
      -- , Vector_ (VectorFor point) d r
      ) => Point_ point d r | point -> d
                            , point -> r where

  -- | Construct a point from a vector
  fromVector :: Vector_ vector d r => vector -> point

  -- | Traversal over *all* coordinates of the points. Coordinates are 1-indexed.
  --
  -- >>> itraverseOf coordinates (\i x -> print (i,x)) (Point2 10 20 :: Point 2 Int)
  -- (1,10)
  -- (2,20)
  -- Point2 () ()
  coordinates :: forall point' s. ( HasVector point point' r s
                                  , HasComponents (VectorFor point) (VectorFor point')
                                  , Point_ point' d s
                                  ) => IndexedTraversal Int point point' r s
  coordinates = vector @point @point' . tr
    where
      tr :: IndexedTraversal Int (VectorFor point) (VectorFor point') r s
      tr = reindexed (+1) components
  {-# INLINE coordinates #-}

  -- | Get the coordinate in a given dimension. Consider using 'coord'
  -- instead, this is just a way of implementing that function more easily.
  --
  -- >>> myPoint ^. coordProxy (Proxy @2)
  -- 2
  -- >>> myPoint & coordProxy (Proxy @1) .~ 10
  -- Point3 10 2 3
  -- >>> myPoint & coordProxy (Proxy @3) %~ (+1)
  -- Point3 1 2 4
  coordProxy    :: (1 <= i, i <= d, KnownNat i)
                => proxy i -> IndexedLens' Int point r
  coordProxy px = singular $ uncheckedCoord (fromIntegral . natVal $ px)
  {-# INLINE coordProxy #-}

  -- | Get the coordinate in a given dimension. This operation is unsafe in the
  -- sense that no bounds are checked. Consider using `coord` instead.
  --
  -- >>> myPoint ^.. uncheckedCoord 2
  -- [2]
  uncheckedCoord   :: Int -> IndexedTraversal' Int point r
  uncheckedCoord i = vector . elem'
    where
      elem' :: IndexedTraversal' Int (VectorFor point) r
      elem' = iix (i - 1)
                -- vectors are 0 indexed, whereas we are 1 indexed.
  {-# INLINE  uncheckedCoord #-}

  {-# MINIMAL fromVector #-}


-- | Get the coordinate in a given dimension
--
-- >>> myPoint ^. coord @2
-- 2
-- >>> myPoint & coord @1 .~ 10
-- Point3 10 2 3
-- >>> myPoint & coord @3 %~ (+1)
-- Point3 1 2 4
coord :: forall i point d r. (1 <= i, i <= d, KnownNat i, Point_ point d r)
      => IndexedLens' Int point r
coord = coordProxy $ Proxy @i
{-# INLINE coord #-}

-- | Convert a generic point into this specific point.
pointFromPoint :: forall point point' d r.
                  ( Point_ point  d r
                  , Point_ point' d r
                  ) => point -> point'
pointFromPoint = fromVector . view vector
{-# INLINE[1] pointFromPoint #-}
{-# RULES
  "pointFromPoint/sameType"
      forall point. forall (p :: point). pointFromPoint @point @point p = p
  #-}

--------------------------------------------------------------------------------

-- | A bidirectional pattern synonym for 1 dimensional points.
pattern Point1_   :: Point_ point 1 r => r -> point
pattern Point1_ x <- (view vector -> Vector1_ x)
  where
    Point1_ x = fromVector (Vector.Vector1 x)
{-# COMPLETE Point1_ #-}

-- | A bidirectional pattern synonym for 2 dimensional points.
pattern Point2_     :: ( Point_ point 2 r
                       -- , ConstructableVector_ (Vector.VectorFamily 2 r) 2 r
                       ) => r -> r -> point
pattern Point2_ x y <- (view vector -> Vector2_ x y)
--  where
--    Point2_ x y = fromVector (Vector.Vector2 x y)
{-# COMPLETE Point2_ #-}


-- | A bidirectional pattern synonym for 3 dimensional points.
pattern Point3_       :: ( Point_ point 3 r
                         -- , ConstructableVector_ (Vector.VectorFamily 3 r) 3 r
                         ) => r -> r -> r -> point
pattern Point3_ x y z <- (view vector -> Vector3_ x y z)
  -- where
  --   Point3_ x y z = fromVector (Vector.Vector3 x y z)
{-# COMPLETE Point3_ #-}

-- | A bidirectional pattern synonym for 4 dimensional points.
pattern Point4_         :: ( Point_ point 4 r
                           -- , ConstructableVector_ (Vector.VectorFamily 4 r) 4 r
                           ) => r -> r -> r -> r -> point
pattern Point4_ x y z w <- (view vector -> Vector4_ x y z w)
  -- where
  --   Point4_ x y z w = fromVector (Vector.Vector4 x y z w)
{-# COMPLETE Point4_ #-}

-- | Point representing the origin in d dimensions
--
-- >>> origin :: Point 4 Int
-- Point4 0 0 0 0
origin :: forall point d r. (Num r, Point_ point d r) => point
origin = fromVector $ zero @(VectorFor point)

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
pointFromList :: forall point d r. (Point_ point d r) => [r] -> Maybe point
pointFromList = fmap (fromVector @point @d @r @(VectorFor point)) . vectorFromList

-- | Project a point down into a lower dimension.
projectPoint :: forall i d r point point'. ( i <= d, KnownNat i
                                           , Point_ point  d r
                                           , Point_ point' i r
                ) => point -> point'
projectPoint = fromVector @point' @i @r @(VectorFor point') . prefix . view vector

--------------------------------------------------------------------------------


-- | Shorthand to access the first coordinate
--
-- >>> myPoint ^. xCoord
-- 1
-- >>> Point2 1 2 & xCoord .~ 10
-- Point2 10 2
xCoord :: (1 <= d, Point_ point d r) => Lens' point r
xCoord = coord @1
{-# INLINABLE xCoord #-}

-- | Shorthand to access the second coordinate
--
-- >>> Point2 1 2 ^. yCoord
-- 2
-- >>> myPoint & yCoord %~ (+1)
-- Point3 1 3 3
yCoord :: (2 <= d, Point_ point d r) => Lens' point r
yCoord = coord @2
{-# INLINABLE yCoord #-}

-- | Shorthand to access the third coordinate
--
-- >>> myPoint ^. zCoord
-- 3
-- >>> myPoint & zCoord %~ (+1)
-- Point3 1 2 4
zCoord :: (3 <= d, Point_ point d r) => Lens' point r
zCoord = coord @3
{-# INLINABLE zCoord #-}

-- | Shorthand to access the fourth coordinate
--
-- >>> (Point4 1 2 3 4 :: Point 4 Int) ^. wCoord
-- 4
-- >>> (Point4 1 2 3 4 :: Point 4 Int) & wCoord %~ (+1)
-- Point4 1 2 3 5
wCoord :: (4 <= d, Point_ point d r) => Lens' point r
wCoord = coord @4
{-# INLINABLE wCoord #-}


--------------------------------------------------------------------------------

-- | The type of points used in a particular data type
type family PointFor t

-- | Data types that store points
class HasPoints s t point point' | s -> point
                                 , t -> point' where
  -- | Traversal over all points in the structure
  allPoints :: ( Point_ point  d r
               , Point_ point' d r'
               , NumType s ~ r
               , NumType t ~ r'
               , Dimension s ~ d, Dimension t ~ d
               ) => Traversal1 s t point point'

type HasPoints' s point = HasPoints s s point point

--------------------------------------------------------------------------------


instance HasVector point point' r s => HasVector (point :+ extra) (point' :+ extra) r s where
  vector = core.vector

instance Affine_ point => Affine_ (point :+ extra) where
  (.-.)   = (.-.) `on` view core
  p .+^ v = p&core %~ (.+^ v)

instance (Point_ point d r, Monoid extra) => Point_ (point :+ extra) d r where
  {-# SPECIALIZE instance Point_ point d r => Point_ (point :+ ()) d r #-}
  fromVector v = fromVector v :+ mempty
