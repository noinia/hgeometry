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
  , HasCoordinates(..)
  , Affine_(..)
  , Point_(..), pattern Point1_, pattern Point2_, pattern Point3_, pattern Point4_
  , ConstructablePoint_(..)
  , origin
  , pointFromList
  , coord
  , xCoord, yCoord, zCoord, wCoord

  -- , projectPoint
  -- , PointFor
  , HasPoints(..), HasPoints'
  , NoDefault(..)
  ) where

import           Control.Lens
import           Data.Default.Class
import           Data.Function (on)
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Proxy (Proxy(..))
import           GHC.Generics (Generic)
import           GHC.TypeNats
import           HGeometry.Ext
import           HGeometry.Properties
import           HGeometry.Vector
import qualified Linear.Affine as Linear

-- $setup
-- >>> import HGeometry.Point
-- >>> :{
-- let myVector :: Vector 3 Int
--     myVector = Vector3 1 2 3
--     myPoint = Point myVector
-- :}


--------------------------------------------------------------------------------

-- | Type class for types, usually points, that have a Lens to interpret the
-- point as a vector.
class HasVector point point' where
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
            , Dimension point' ~  d
            , NumType point' ~ s
            )
         => Lens point point' (Vector d r) (Vector d s)

type instance Dimension (Linear.Point v r) = Dimension (v r)
type instance NumType (Linear.Point v r)   = r

instance ( Vector_ (v r) d r
         , Vector_ (v s) d s
         ) => HasVector (Linear.Point v r) (Linear.Point v s) where
  vector = lens (\(Linear.P v) -> v^._Vector)
                (\_ v -> Linear.P $ v^.from _Vector)
  {-# INLINE vector #-}

-- | Class for point types that have a type changing traversal over
-- all coordinates.
class ( Has_ Vector_ (Dimension point) (NumType point)
      , Has_ Vector_ (Dimension point') (NumType point')
      , HasComponents (Vector (Dimension point') (NumType point))
                      (Vector (Dimension point') (NumType point'))
      , Dimension point ~ Dimension point'
      , HasVector point point'
      )
      => HasCoordinates point point' where
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
  coordinates :: IndexedTraversal1 Int point point' (NumType point) (NumType point')
  coordinates = vector  . reindexed (+1) components
  {-# INLINE coordinates #-}



-- | Affine space; essentially the same as Linear.Affine, but for
-- points of kind Type rather than (Type -> Type).
class ( Additive_ (Vector d r) d r
      , HasCoordinates point point
      , d ~ Dimension point
      , r ~ NumType point
      ) => Affine_ point d r | point -> d
                             , point -> r where
  {-# MINIMAL #-}

  -- | p .-. q represents the vector from q to p
  (.-.) :: Num r => point -> point -> Vector d r
  default (.-.) :: (HasVector point point, Num r) => point -> point -> Vector d r
  p .-. q = (p^.vector) ^-^ (q^.vector)
  {-# INLINE (.-.) #-}

  -- | add a vector to a point
  --
  -- >>> myPoint .+^ Vector3 100 200 300
  -- Point3 101 202 303
  (.+^) :: Num r => point -> Vector d r -> point
  default (.+^) :: (HasVector point point, Num r) => point -> Vector d r -> point
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
         , s ~ IxValue (v s)
         , d ~ Dimension (v s)
         , Vector_ (v r) d r
         , Vector_ (v s) d s
         , Has_ Vector_ d r
         , Has_ Vector_ d s
         , HasComponents (Vector d r) (Vector d s)
         ) => HasCoordinates (Linear.Point v r) (Linear.Point v s)

instance ( d ~ Dimension (v r)
         , r ~ IxValue (v r)
         , Vector_ (v r) d r
         , Additive_ (Vector d r) d r
         ) => Affine_ (Linear.Point v r) d r where

--------------------------------------------------------------------------------

-- | A class representing points in d-dimensional space.
class ( Affine_ point d r
      , HasVector point point
      ) => Point_ point d r where
  {-# MINIMAL #-}

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


-- | Type class for constructable points
class Point_ point d r => ConstructablePoint_ point d r where
  {-# MINIMAL fromVector #-}

  -- | Construct a point from a vector
  --
  -- >>> fromVector (Vector4 1 2 3 4) :: Point 4 Int
  -- Point4 1 2 3 4
  fromVector :: Vector d r -> point


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
         , Vector_ (v r) d r
         , Additive_ (Vector d r) d r
         ) => Point_ (Linear.Point v r) d r

instance ( d ~ Dimension (v r)
         , r ~ IxValue (v r)
         , Vector_ (v r) d r
         , Additive_ (Vector d r) d r
         ) => ConstructablePoint_ (Linear.Point v r) d r where
  fromVector = Linear.P . review _Vector
  {-# INLINE fromVector #-}

-- | Point representing the origin in d dimensions
--
-- >>> origin :: Point 4 Int
-- Point4 0 0 0 0
origin :: forall point d r. (Num r, ConstructablePoint_ point d r) => point
origin = fromVector zero
{-# INLINE origin #-}

-- | A pattern synonym for 1 dimensional points.
pattern Point1_   :: Point_ point 1 r => r -> point
pattern Point1_ x <- (view xCoord -> x)
--   where
--     Point1_ x = fromVector $ Vector1 x
-- {-# INLINE Point1_ #-}
{-# COMPLETE Point1_ #-}

-- | A pattern synonym for 2 dimensional points.
pattern Point2_     :: ( Point_ point 2 r
                       ) => r -> r -> point
pattern Point2_ x y <- (view vector -> Vector2 x y)
--  where
--    Point2_ x y = fromVector $ Vector2 x y
-- {-# INLINE Point2_ #-}
{-# COMPLETE Point2_ #-}


-- | A pattern synonym for 3 dimensional points.
pattern Point3_       :: ( Point_ point 3 r
                         ) => r -> r -> r -> point
pattern Point3_ x y z <- (view vector -> Vector3 x y z)
--   where
--     Point3_ x y z = fromVector $ Vector3 x y z
-- {-# INLINE Point3_ #-}
{-# COMPLETE Point3_ #-}

-- | A bidirectional pattern synonym for 4 dimensional points.
pattern Point4_         :: ( Point_ point 4 r
                           ) => r -> r -> r -> r -> point
pattern Point4_ x y z w <- (view vector -> Vector4 x y z w)
--   where
--     Point4_ x y z w = fromVector $ Vector4 x y z w
-- {-# INLINE Point4_ #-}
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
pointFromList :: ( ConstructablePoint_ point d r
                 , Vector_ (Vector d r) d r
                 ) => [r] -> Maybe point
pointFromList = fmap fromVector . vectorFromList
{-# INLINE pointFromList #-}

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
  -- >>> let xs = NonEmpty.fromList [Point2 10 10, Point2 20 (30 :: Int)]
  -- >>> xs^..allPoints
  -- [Point2 10 10,Point2 20 30]
  -- >>> over allPoints (.+^ Vector2 10 10) xs :: NonEmpty.NonEmpty (Point 2 Int)
  -- Point2 20 20 :| [Point2 30 40]
  allPoints :: ( Point_ point  d r
               , Point_ point' d r'
               , NumType s ~ r
               , NumType t ~ r'
               , Dimension s ~ d, Dimension t ~ d
               ) => Traversal1 s t point point'

instance HasPoints (NonEmpty.NonEmpty point) (NonEmpty.NonEmpty point') point point' where
  allPoints = traverse1

-- | Shorthand for 'HasPoints s s point point'
type HasPoints' s point = HasPoints s s point point

instance HasPoints (Linear.Point v r) (Linear.Point v' r')
                   (Linear.Point v r) (Linear.Point v' r') where
  allPoints = id


--------------------------------------------------------------------------------

instance HasVector point point' => HasVector (point :+ extra) (point' :+ extra) where
  vector = core.vector
  {-# INLINE vector #-}

instance HasCoordinates point point' => HasCoordinates (point :+ extra) (point' :+ extra) where
  coordinates = core.coordinates
  {-# INLINE coordinates #-}

instance Affine_ point d r => Affine_ (point :+ extra) d r where
  (.-.)   = (.-.) `on` view core
  {-# INLINE (.-.) #-}
  p .+^ v = p&core %~ (.+^ v)
  {-# INLINE (.+^) #-}

instance (Point_ point d r, Default extra) => Point_ (point :+ extra) d r where
  {-# SPECIALIZE instance Point_ point d r => Point_ (point :+ ()) d r #-}

instance (ConstructablePoint_ point d r, Default extra)
          => ConstructablePoint_ (point :+ extra) d r where
  {-# SPECIALIZE instance ConstructablePoint_ point d r
                          => ConstructablePoint_ (point :+ ()) d r #-}
  fromVector v = fromVector v :+ def

-- | A newtype that can discharge the Default constraint in an unsafe way, if you really
-- sure that you'll never actually need the default
newtype NoDefault extra = NoDefault extra
  deriving newtype (Show,Read,Eq,Ord,Enum,Num,Bounded,Real,Fractional,RealFrac,Generic)

instance Default (NoDefault extra) where
  def = error "NoDefault does not have an actual default. So something went wrong"
