{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module HGeometry.Point.Class where

import           Control.Lens hiding (elements, element)
import           Data.Proxy (Proxy(..))
import           GHC.TypeNats
import           HGeometry.Properties
import qualified HGeometry.Vector as Vector
import           HGeometry.Vector.Class

--------------------------------------------------------------------------------

-- -- $setup
-- -- >>> import HGeometry.Point(Point(Point,Point1,Point2,Point3,Point4))

class ( NumType point ~ r
      , NumType point' ~ s
      , Vector_ (Diff_ point) (Dimension point) r
      ) => HasVector point point' r s where
  type Diff_ point

  -- | Lens to access the vector corresponding to this point.
  --
  -- >>> (Point3 1 2 3) ^. vector
  -- Vector3 1 2 3
  -- >>> origin & vector .~ Vector3 1 2 3
  -- Point3 1 2 3
  vector :: ( Dimension point ~ d, Dimension point' ~ d)
         => Lens point point' (Diff_ point) (Diff_ point')

-- | Affine space; essentially the same as Linear.Affine, but for
-- points of kind Type rather than (Type -> Type).
class ( Vector_ (Diff_ point) (Dimension point) (NumType point)
      , Additive_ (Diff_ point)
      ) => Affine_ point where

  -- | p .-. q represents the vector from q to p
  (.-.) :: Num (NumType point) => point -> point -> Diff_ point

  -- | add a vector to a point
  (.+^) :: Num (NumType point) => point -> Diff_ point -> point

  -- | subtract a vector from a point
  (.-^) :: Num (NumType point) => point -> Diff_ point -> point
  p .-^ v = p .+^ negated v
  {-# MINIMAL (.-.), (.+^) #-}

class ( Dimension point ~ d
      , NumType point   ~ r
      , HasVector point point r r
      , Affine_ point
      ) => Point_ point d r | point -> d
                            , point -> r where

  -- | Construct a point from a vector
  fromVector :: Vector_ vector d r => vector -> point

  -- | Traversal over *all* coordinates of the points. Coordinates are 1-indexed.
  --
  -- >>> itraverseOf coordinates (\i x -> print (i,x)) (Point2 10 20)
  -- (1,10)
  -- (2,20)
  -- Point2 () ()
  coordinates :: forall point' s. ( HasVector point point' r s
                                  , HasElements (Diff_ point) (Diff_ point')
                                  , Point_ point' d s
                                  ) => IndexedTraversal Int point point' r s
  coordinates = vector @point @point' . tr
    where
      tr :: IndexedTraversal Int (Diff_ point) (Diff_ point') r s
      tr = reindexed (+1) elements
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
                => proxy i -> IndexedLens' Int point r
  coordProxy px = singular $ uncheckedCoord (fromIntegral . natVal $ px)
  {-# INLINE coordProxy #-}

  -- | Get the coordinate in a given dimension. This operation is unsafe in the
  -- sense that no bounds are checked. Consider using `coord` instead.
  --
  --
  -- >>> Point3 1 2 3 ^. uncheckedCoord 2
  -- 2
  uncheckedCoord   :: Int -> IndexedTraversal' Int point r
  uncheckedCoord i = vector . elem'
    where
      elem' :: IndexedTraversal' Int (Diff_ point) r
      elem' = iix (i - 1)
                -- vectors are 0 indexed, whereas we are 1 indexed.
  {-# INLINE  uncheckedCoord #-}

  {-# MINIMAL fromVector #-}


-- | Get the coordinate in a given dimension
--
-- >>> Point3 1 2 3 ^. coord @2
-- 2
-- >>> Point3 1 2 3 & coord @1 .~ 10
-- Point3 10 2 3
-- >>> Point3 1 2 3 & coord @3 %~ (+1)
-- Point3 1 2 4
coord :: forall i point d r. (1 <= i, i <= d, KnownNat i, Point_ point d r)
      => IndexedLens' Int point r
coord = coordProxy $ Proxy @i
{-# INLINE coord #-}

--------------------------------------------------------------------------------

-- | A bidirectional pattern synonym for 1 dimensional points.
pattern Point1_   :: Point_ point 1 r => r -> point
pattern Point1_ x <- (view vector -> Vector1_ x)
  where
    Point1_ x = fromVector (Vector.Vector1 x)

-- | A bidirectional pattern synonym for 2 dimensional points.
pattern Point2_     :: Point_ point 2 r => r -> r -> point
pattern Point2_ x y <- (view vector -> Vector2_ x y)
  where
    Point2_ x y = fromVector (Vector.Vector2 x y)


-- | A bidirectional pattern synonym for 3 dimensional points.
pattern Point3_       :: Point_ point 3 r => r -> r -> r -> point
pattern Point3_ x y z <- (view vector -> Vector3_ x y z)
  where
    Point3_ x y z = fromVector (Vector.Vector3 x y z)

-- | A bidirectional pattern synonym for 4 dimensional points.
pattern Point4_         :: Point_ point 4 r => r -> r -> r -> r -> point
pattern Point4_ x y z w <- (view vector -> Vector4_ x y z w)
  where
    Point4_ x y z w = fromVector (Vector.Vector4 x y z w)

-- | Point representing the origin in d dimensions
--
-- >>> origin :: Point 4 Int
-- Point4 0 0 0 0
origin :: forall point d r. (Num r, Point_ point d r) => point
origin = fromVector $ zero @(Diff_ point)

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
pointFromList = fmap (fromVector @point @d @r @(Diff_ point)) . vectorFromList

-- | Project a point down into a lower dimension.
projectPoint :: forall i d r point point'. ( i <= d, KnownNat i
                                           , Point_ point  d r
                                           , Point_ point' i r
                ) => point -> point'
projectPoint = fromVector @point' @i @r @(Diff_ point') . prefix . view vector

--------------------------------------------------------------------------------


-- | Shorthand to access the first coordinate
--
-- >>> Point3 1 2 3 ^. xCoord
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
-- >>> Point3 1 2 3 & yCoord %~ (+1)
-- Point3 1 3 3
yCoord :: (2 <= d, Point_ point d r) => Lens' point r
yCoord = coord @2
{-# INLINABLE yCoord #-}

-- | Shorthand to access the third coordinate
--
-- >>> Point3 1 2 3 ^. zCoord
-- 3
-- >>> Point3 1 2 3 & zCoord %~ (+1)
-- Point3 1 2 4
zCoord :: (3 <= d, Point_ point d r) => Lens' point r
zCoord = coord @3
{-# INLINABLE zCoord #-}

-- | Shorthand to access the fourth coordinate
--
-- >>> Point4 1 2 3 4 ^. wCoord
-- 4
-- >>> Point4 1 2 3 4 & wCoord %~ (+1)
-- Point4 1 2 3 5
wCoord :: (4 <= d, Point_ point d r) => Lens' point r
wCoord = coord @4
{-# INLINABLE wCoord #-}


--------------------------------------------------------------------------------

-- | Data types that store points
class HasPoints s t point point' where
  -- | Traversal over all points in the structure
  allPoints :: ( Point_ point  d r
               , Point_ point' d r'
               , NumType s ~ r
               , NumType t ~ r'
               , Dimension s ~ d, Dimension t ~ d
               ) => Traversal s t point point'
