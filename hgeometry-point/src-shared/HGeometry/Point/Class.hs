{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DefaultSignatures #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Point.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A class of types that can act as \(d\)-dimensional points.
--
--------------------------------------------------------------------------------
module HGeometry.Point.Class
  ( HasVector(..)
  , Affine_(..)
  , Point_(..), pattern Point1_, pattern Point2_, pattern Point3_, pattern Point4_
  , origin
  -- , pointFromPoint
  , pointFromList
  , coord
  , xCoord, yCoord, zCoord, wCoord

  -- , projectPoint
  -- , PointFor
  , HasPoints(..), HasPoints'
  ) where

import           Control.Arrow ((&&&))
import           Control.Lens
-- import           Data.Ext
-- import           Data.Function (on)
import           Data.Proxy (Proxy(..))
import           GHC.TypeNats
import           HGeometry.Properties
import           HGeometry.Vector.Class
-- import           HGeometry.Vector
import qualified Linear.Affine as Linear
import           Linear.V2 (V2(..))
import           Linear.V3 (V3(..))
import           Linear.V4 (V4(..))

--------------------------------------------------------------------------------

-- | Types that have a 'vector' field lens
class HasVector point where

  -- | Lens to access the vector corresponding to this point.
  --
  -- >>> myPoint ^. vector
  -- Vector3 1 2 3
  -- >>> ( myPoint & vector .~ Vector3 3 2 1  ) :: Point 3 Int
  -- Point3 3 2 1
  -- >>> (myPoint & coordinates %~ show ) :: Point 3 String
  -- Point3 "1" "2" "3"
  vector :: ( Dimension point ~ d
            , NumType point ~ r
            )
         => Lens' point (Vector d r)

type instance Dimension (Linear.Point v r) = Dimension (v r)
type instance NumType (Linear.Point v r)   = r


instance ( Vector_ (v r)
         , IxValue (v r) ~ r
         ) => HasVector (Linear.Point v r) where
  vector = Linear._Point._Vector
  {-# INLINE vector #-}

-- | Affine space; essentially the same as Linear.Affine, but for
-- points of kind Type rather than (Type -> Type).
class ( Additive_ (Vector d r)
      , d ~ Dimension point, r ~ NumType point
      ) => Affine_ point d r | point -> d
                             , point -> r where
  {-# MINIMAL #-}

  -- | p .-. q represents the vector from q to p
  (.-.) :: Num r => point -> point -> Vector d r
  default (.-.) :: (HasVector point, Num r) => point -> point -> Vector d r
  p .-. q = (p^.vector) ^-^ (q^.vector)
  {-# INLINE (.-.) #-}

  -- | add a vector to a point
  --
  -- >>> myPoint .+^ Vector3 100 200 300
  -- Point3 101 202 303
  (.+^) :: Num r => point -> Vector d r -> point
  default (.+^) :: (HasVector point, Num r) => point -> Vector d r -> point
  p .+^ v = p&vector %~ (^+^ v)
  {-# INLINE (.+^) #-}

  -- | subtract a vector from a point
  --
  -- >>> myPoint .-^ Vector3 100 200 300
  -- Point3 (-99) (-198) (-297)
  (.-^) :: Num r => point -> Vector d r -> point
  p .-^ v = p .+^ negated v
  {-# INLINE (.-^) #-}

instance ( d ~ Dimension (v r)
         , r ~ IxValue (v r)
         , Vector_ (v r)
         , Additive_ (Vector d r)
         ) => Affine_ (Linear.Point v r) d r where


--------------------------------------------------------------------------------

-- | A class representing points in d-dimensional space.
class ( Dimension point ~ d
      , NumType point   ~ r
      , HasVector point
      , Affine_ point d r
      -- , Vector_ (VectorFor point) d r
      ) => Point_ point d r | point -> d
                            , point -> r where
  {-# MINIMAL fromVector #-}

  -- | Construct a point from a vector
  --
  -- >>> fromVector (Vector4 1 2 3 4) :: Point 4 Int
  -- Point4 1 2 3 4
  fromVector :: Vector d r -> point

  -- | Traversal over *all* coordinates of the points. Coordinates are 1-indexed.
  --
  -- >>> imapMOf_ coordinates (\i x -> print (i,x)) (Point2 10 20 :: Point 2 Int)
  -- (1,10)
  -- (2,20)
  -- >>> itraverseOf coordinates (\i x -> print (i,x)) (Point2 10 20) :: IO (Point 2 ())
  -- (1,10)
  -- (2,20)
  -- Point2 () ()
  -- >>> over coordinates (+1) $ Point2 10 20 :: Point 2 Int
  -- Point2 11 21
  coordinates :: IndexedTraversal1 Int point point r r
  coordinates = vector  . reindexed (+1) components
    -- where
      -- tr :: IndexedTraversal Int (Vector d ) (VectorFor point') r s
      -- tr =
  {-# INLINE coordinates #-}

  -- | Get the coordinate in a given dimension. This operation is unsafe in the
  -- sense that no bounds are checked. Consider using `coord` instead.
  --
  -- >>> myPoint ^.. coord' 2
  -- [2]
  coord'   :: Int -> IndexedTraversal' Int point r
  coord' i = vector . elem'
    where
      -- elem' :: IndexedTraversal' Int (VectorFor point) r
      elem' = component' (i - 1)
                -- vectors are 0 indexed, whereas we are 1 indexed.
  {-# INLINE coord' #-}



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
coord = singular $ coord' (fromIntegral . natVal $ Proxy @i)
{-# INLINE coord #-}


instance ( d ~ Dimension (v r)
         , r ~ IxValue (v r)
         , Vector_ (v r)
         , Additive_ (Vector d r)
         ) => Point_ (Linear.Point v r) d r where
  fromVector = Linear.P . review _Vector


-- | Point representing the origin in d dimensions
--
-- >>> origin :: Point 4 Int
-- Point4 0 0 0 0
origin :: forall point d r. (Num r, Point_ point d r) => point
origin = fromVector zero


-- | A bidirectional pattern synonym for 1 dimensional points.
pattern Point1_   :: Point_ point 1 r => r -> point
pattern Point1_ x <- (view xCoord -> x)
  where
    Point1_ x = fromVector (generate $ const x)
{-# COMPLETE Point1_ #-}

-- | A bidirectional pattern synonym for 2 dimensional points.
pattern Point2_     :: ( Point_ point 2 r
                       ) => r -> r -> point
pattern Point2_ x y <- (view xCoord &&& view yCoord -> (x,y))
 where
   Point2_ x y = fromVector . view _Vector $ V2 x y
{-# COMPLETE Point2_ #-}

-- | A bidirectional pattern synonym for 3 dimensional points.
pattern Point3_       :: ( Point_ point 3 r
                         -- , ConstructableVector_ (Vector.VectorFamily 3 r) 3 r
                         ) => r -> r -> r -> point
pattern Point3_ x y z <- (view xCoord &&& view yCoord &&& view zCoord -> (x,(y,z)))
  where
    Point3_ x y z = fromVector . view _Vector $ V3 x y z
{-# COMPLETE Point3_ #-}

-- | A bidirectional pattern synonym for 4 dimensional points.
pattern Point4_         :: ( Point_ point 4 r
                           -- , ConstructableVector_ (Vector.VectorFamily 4 r) 4 r
                           ) => r -> r -> r -> r -> point
pattern Point4_ x y z w <- (view xCoord &&& view yCoord &&& view zCoord &&& view wCoord
                           -> (x,(y,(z,w))))
  where
    Point4_ x y z w = fromVector . view _Vector $ V4 x y z w
{-# COMPLETE Point4_ #-}


-- | Constructs a point from a list of coordinates. The length of the
-- list has to match the dimension exactly.
--
-- >>> pointFromList [1,2,3] :: Maybe (Point 3 Int)
-- Just (Point3 1 2 3)
-- >>> pointFromList [1] :: Maybe (Point 3 Int)
-- Nothing
-- >>> pointFromList [1,2,3,4] :: Maybe (Point 3 Int)
-- Nothing
pointFromList :: ( Point_ point d r
                 , VectorLike_ (Vector d r)
                 ) => [r] -> Maybe point
pointFromList = fmap fromVector . vectorFromList

--------------------------------------------------------------------------------

-- | Shorthand to access the first coordinate
--
-- >>> myPoint ^. xCoord
-- 1
-- >>> Point2 1 (2 :: Int) & xCoord .~ 10
-- Point2 10 2
xCoord :: (1 <= d, Point_ point d r) => IndexedLens' Int point r
xCoord = coord @1
{-# INLINABLE xCoord #-}

-- | Shorthand to access the second coordinate
--
-- >>> Point2 1 (2 :: Int) ^. yCoord
-- 2
-- >>> myPoint & yCoord %~ (+1)
-- Point3 1 3 3
yCoord :: (2 <= d, Point_ point d r) => IndexedLens' Int point r
yCoord = coord @2
{-# INLINABLE yCoord #-}

-- | Shorthand to access the third coordinate
--
-- >>> myPoint ^. zCoord
-- 3
-- >>> myPoint & zCoord %~ (+1)
-- Point3 1 2 4
zCoord :: (3 <= d, Point_ point d r) => IndexedLens' Int point r
zCoord = coord @3
{-# INLINABLE zCoord #-}

-- | Shorthand to access the fourth coordinate
--
-- >>> (Point4 1 2 3 4 :: Point 4 Int) ^. wCoord
-- 4
-- >>> (Point4 1 2 3 4 :: Point 4 Int) & wCoord %~ (+1)
-- Point4 1 2 3 5
wCoord :: (4 <= d, Point_ point d r) => IndexedLens' Int point r
wCoord = coord @4
{-# INLINABLE wCoord #-}

--------------------------------------------------------------------------------

-- | Data types that store points
class HasPoints s t point point' | s -> point
                                 , t -> point' where
  -- | Traversal over all points in the structure
  --
  -- >>> let seg = ClosedLineSegment (Point2 10 10) (Point2 20 (30 :: Int))
  -- >>> seg^..allPoints
  -- [Point2 10 10,Point2 20 30]
  -- >>> over allPoints (.+^ Vector2 10 10) seg :: ClosedLineSegment (Point 2 Int)
  -- ClosedLineSegment (Point2 20 20) (Point2 30 40)
  allPoints :: ( Point_ point  d r
               , Point_ point' d r'
               , NumType s ~ r
               , NumType t ~ r'
               , Dimension s ~ d, Dimension t ~ d
               ) => Traversal1 s t point point'

-- | Shorthand for 'HasPoints s s point point'
type HasPoints' s point = HasPoints s s point point

instance HasPoints (Linear.Point v r) (Linear.Point v' r')
                   (Linear.Point v r) (Linear.Point v' r') where
  allPoints = id
