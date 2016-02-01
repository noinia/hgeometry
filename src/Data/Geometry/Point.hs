{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
module Data.Geometry.Point where

import           Control.Applicative
import           Data.Monoid(mconcat)
import           Control.Lens
import           Data.Proxy

import qualified Data.Traversable as T
import qualified Data.Foldable as F
import qualified Data.Vector.Fixed as FV
import qualified Data.List as L

import           Data.Ext
import           Data.Geometry.Properties
import           Data.Geometry.Vector
import           GHC.TypeLits

import qualified Data.Geometry.Vector as Vec
import qualified Data.CircularList as C
import qualified Data.CircularList.Util as CU

--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- $setup
-- >>> :{
-- let myVector :: Vector 3 Int
--     myVector = v3 1 2 3
--     myPoint = Point myVector
-- :}


--------------------------------------------------------------------------------
-- * A d-dimensional Point

-- | A d-dimensional point.
newtype Point d r = Point { toVec :: Vector d r }

instance (Show r, Arity d) => Show (Point d r) where
  show (Point (Vector v)) = mconcat [ "Point", show $ FV.length v , " "
                                    , show $ F.toList v
                                    ]



deriving instance (Eq r, Arity d)   => Eq (Point d r)
deriving instance (Ord r, Arity d)  => Ord (Point d r)
deriving instance Arity d           => Functor (Point d)
deriving instance Arity d           => F.Foldable (Point d)
deriving instance Arity d           => T.Traversable (Point d)


type instance NumType (Point d r) = r
type instance Dimension (Point d r) = d

instance Arity d =>  Affine (Point d) where
  type Diff (Point d) = Vector d

  p .-. q = toVec p ^-^ toVec q
  p .+^ v = Point $ toVec p ^+^ v


-- | Point representing the origin in d dimensions
--
-- >>> origin :: Point 4 Int
-- Point4 [0,0,0,0]
origin :: (Arity d, Num r) => Point d r
origin = Point $ pure 0


-- ** Accessing points

-- | Lens to access the vector corresponding to this point.
--
-- >>> (point3 1 2 3) ^. vector
-- Vector3 [1,2,3]
-- >>> origin & vector .~ v3 1 2 3
-- Point3 [1,2,3]
vector :: Lens' (Point d r) (Vector d r)
vector = lens toVec (const Point)


-- | Get the coordinate in a given dimension. This operation is unsafe in the
-- sense that no bounds are checked. Consider using `coord` instead.
--
--
-- >>> point3 1 2 3 ^. unsafeCoord 2
-- 2
unsafeCoord   :: Arity d => Int -> Lens' (Point d r) r
unsafeCoord i = vector . FV.element (i-1)
                -- Points are 1 indexed, vectors are 0 indexed

-- | Get the coordinate in a given dimension
--
-- >>> point3 1 2 3 ^. coord (C :: C 2)
-- 2
-- >>> point3 1 2 3 & coord (C :: C 1) .~ 10
-- Point3 [10,2,3]
-- >>> point3 1 2 3 & coord (C :: C 3) %~ (+1)
-- Point3 [1,2,4]
coord   :: forall proxy i d r. (Index' (i-1) d, Arity d) => proxy i -> Lens' (Point d r) r
coord _ = vector . Vec.element (Proxy :: Proxy (i-1))




--------------------------------------------------------------------------------
-- * Convenience functions to construct 2 and 3 dimensional points


-- | We provide pattern synonyms Point2 and Point3 for 2 and 3 dimensional points. i.e.
-- we can write:
--
-- >>> :{
--   let
--     f              :: Point 2 r -> r
--     f (Point2 x y) = x
--   in f (point2 1 2)
-- :}
-- 1
--
-- if we want.
pattern Point2 x y   <- (_point2 -> (x,y))

-- | Similarly, we can write:
--
-- >>> :{
--   let
--     g                :: Point 3 r -> r
--     g (Point3 x y z) = z
--   in g myPoint
-- :}
-- 3
pattern Point3 x y z <- (_point3 -> (x,y,z))

-- | Construct a 2 dimensional point
--
-- >>> point2 1 2
-- Point2 [1,2]
point2     :: r -> r -> Point 2 r
point2 x y = Point $ v2 x y

-- | Destruct a 2 dimensional point
--
-- >>> _point2 $ point2 1 2
-- (1,2)
_point2   :: Point 2 r -> (r,r)
_point2 p = (p^.xCoord, p^.yCoord)


-- | Construct a 3 dimensional point
--
-- >>> point3 1 2 3
-- Point3 [1,2,3]
point3       :: r -> r -> r -> Point 3 r
point3 x y z = Point $ v3 x y z

-- | Destruct a 3 dimensional point
--
-- >>> _point3 $ point3 1 2 3
-- (1,2,3)
_point3   :: Point 3 r -> (r,r,r)
_point3 p = (p^.xCoord, p^.yCoord, p^.zCoord)


type i <=. d = (Index' (i-1) d, Arity d)

-- | Shorthand to access the first coordinate C 1
--
-- >>> point3 1 2 3 ^. xCoord
-- 1
-- >>> point2 1 2 & xCoord .~ 10
-- Point2 [10,2]
xCoord :: (1 <=. d) => Lens' (Point d r) r
xCoord = coord (C :: C 1)

-- | Shorthand to access the second coordinate C 2
--
-- >>> point2 1 2 ^. yCoord
-- 2
-- >>> point3 1 2 3 & yCoord %~ (+1)
-- Point3 [1,3,3]
yCoord :: (2 <=. d) => Lens' (Point d r) r
yCoord = coord (C :: C 2)

-- | Shorthand to access the third coordinate C 3
--
-- >>> point3 1 2 3 ^. zCoord
-- 3
-- >>> point3 1 2 3 & zCoord %~ (+1)
-- Point3 [1,2,4]
zCoord :: (3 <=. d) => Lens' (Point d r) r
zCoord = coord (C :: C 3)



--------------------------------------------------------------------------------
-- * Point Functors

-- | Types that we can transform by mapping a function on each point in the structure
class PointFunctor g where
  pmap :: (Point (Dimension (g r)) r -> Point (Dimension (g s)) s) -> g r -> g s

  -- pemap :: (d ~ Dimension (g r)) => (Point d r :+ p -> Point d s :+ p) -> g r -> g s
  -- pemap =

instance PointFunctor (Point d) where
  pmap f = f


--------------------------------------------------------------------------------
-- * Functions specific to Two Dimensional points

data CCW = CCW | CoLinear | CW
         deriving (Show,Eq)

-- | Given three points p q and r determine the orientation when going from p to r via q.
ccw :: (Ord r, Num r) => Point 2 r -> Point 2 r -> Point 2 r -> CCW
ccw p q r = case z `compare` 0 of
              LT -> CW
              GT -> CCW
              EQ -> CoLinear
     where
       Vector2 ux uy = q .-. p
       Vector2 vx vy = r .-. p
       z             = ux * vy - uy * vx

sortArround'   :: (Ord r, Num r)
               => Point 2 r :+ p -> [Point 2 r :+ p] -> [Point 2 r :+ p]
sortArround' c = L.sortBy (ccwCmpAround c)


-- | Sort the points arround the given point p in counter clockwise order with
-- respect to the rightward horizontal ray starting from p.  If two points q
-- and r are colinear with p, the closest one to p is reported first.
-- running time: O(n log n)
sortArround       :: (Ord r, Num r)
                  => Point 2 r :+ p -> [Point 2 r :+ p] -> [Point 2 r :+ p]
sortArround p pts = concatMap sortArround' [ topR, topL, bottomL, bottomR ]
  where
    (topL, topR, bottomL, bottomR) = partitionIntoQuadrants p pts
    sortArround' = L.sortBy cmp

    cmp q r = case ccw (p^.core) (q^.core) (r^.core) of
                CCW      -> LT
                CW       -> GT
                CoLinear -> qdA (p^.core) (q^.core) `compare` qdA (p^.core) (r^.core)

-- | Quadrants of two dimensional points. in CCW order
data Quadrant = TopRight | TopLeft | BottomLeft | BottomRight
              deriving (Show,Read,Eq,Ord,Enum,Bounded)


-- | Quadrants around point c; quadrants are closed on their "previous"
-- boundary (i..e the boundary with the previous quadrant in the CCW order),
-- open on next boundary. The origin itself is assigned the topRight quadrant
quadrantWith                   :: (Arity d, Ord r, 1 <=. d, 2 <=. d)
                               => Point d r :+ q -> Point d r :+ p -> Quadrant
quadrantWith (c :+ _) (p :+ _) = case ( (c^.xCoord) `compare` (p^.xCoord)
                                      , (c^.yCoord) `compare` (p^.yCoord) ) of
                                   (EQ, EQ) -> TopRight
                                   (LT, EQ) -> TopRight
                                   (LT, LT) -> TopRight
                                   (EQ, LT) -> TopLeft
                                   (GT, LT) -> TopLeft
                                   (GT, EQ) -> BottomLeft
                                   (GT, GT) -> BottomLeft
                                   (EQ, GT) -> BottomRight
                                   (LT, GT) -> BottomRight

-- | Quadrants with respect to the origin
quadrant :: (Arity d, Ord r, Num r, 1 <=. d, 2 <=. d) => Point d r :+ p -> Quadrant
quadrant = quadrantWith (ext origin)


-- | Given a center point c, and a set of points, partition the points into
-- quadrants around c (based on their x and y coordinates). The quadrants are
-- reported in the order topLeft, topRight, bottomLeft, bottomRight. The points
-- are in the same order as they were in the original input lists.
-- Points with the same x-or y coordinate as p, are "rounded" to above.
partitionIntoQuadrants       :: (Ord r, Arity d, 1 <=. d, 2 <=. d)
                             => Point d r :+ q
                             -> [Point d r :+ p]
                             -> ( [Point d r :+ p], [Point d r :+ p]
                                , [Point d r :+ p], [Point d r :+ p]
                                )
partitionIntoQuadrants c pts = (topL, topR, bottomL, bottomR)
  where
    (below',above')   = L.partition (on yCoord) pts
    (bottomL,bottomR) = L.partition (on xCoord) below'
    (topL,topR)       = L.partition (on xCoord) above'

    on l q       = q^.core.l < c^.core.l

-- | Counter clockwise ordering of the points around c. Points are ordered with
-- respect to the positive x-axis.
-- Points nearer to the center come before
-- points further away.
ccwCmpAround       :: (Num r, Ord r)
                   => Point 2 r :+ qc -> Point 2 r :+ p -> Point 2 r :+ q -> Ordering
ccwCmpAround c q r = case (quadrantWith c q `compare` quadrantWith c r) of
                       EQ -> case ccw (c^.core) (q^.core) (r^.core) of
                         CCW      -> LT
                         CW       -> GT
                         CoLinear -> qdA (c^.core) (q^.core)
                                     `compare`
                                     qdA (c^.core) (r^.core)
                       x -> x -- if the quadrant differs, use the order
                              -- specified by the quadrant.

-- | Clockwise ordering of the points around c. Points are ordered with
-- respect to the positive x-axis. Points nearer to the center come before
-- points further away.
cwCmpAround       :: (Num r, Ord r)
                  => Point 2 r :+ qc -> Point 2 r :+ p -> Point 2 r :+ q -> Ordering
cwCmpAround c q r = case (quadrantWith c q `compare` quadrantWith c r) of
                       EQ -> case ccw (c^.core) (q^.core) (r^.core) of
                         CCW      -> GT
                         CW       -> LT
                         CoLinear -> qdA (c^.core) (q^.core)
                                     `compare`
                                     qdA (c^.core) (r^.core)
                       LT -> GT
                       GT -> LT -- if the quadrant differs, use the order
                                -- specified by the quadrant.



-- | Given a center c, a new point p, and a list of points ps, sorted in
-- counter clockwise order around c. Insert p into the cyclic order. The focus
-- of the returned cyclic list is the new point p.
--
-- running time: O(n)
insertIntoCyclicOrder   :: (Ord r, Num r)
                        => Point 2 r :+ q -> Point 2 r :+ p
                        -> C.CList (Point 2 r :+ p) -> C.CList (Point 2 r :+ p)
insertIntoCyclicOrder c = CU.insertOrdBy (ccwCmpAround c)


  -- C.fromList $ case (on xCoord p, on yCoord p) of
  --   (False, True)  -> (insert' topL) ++ bottomL ++ bottomR ++ topR
  --   (True, True)   -> (insert' topR) ++ topL ++ bottomL ++ bottomR
  --   (False, False) -> (insert' bottomL) ++ bottomR ++ topR ++ topL
  --   (True, False)  -> (insert' bottomR) ++ topR ++ topL ++ bottomL
  -- where
  --   (topL, topR, bottomL, bottomR) = partitionIntoQuadrants p $ C.leftElements pts
  --   on l q = q^.core.l < c^.core.l
  --   insert' = L.insertBy (ccwCmpAround c) p


-- -- | given c, p, and a list of points pts in counter-clockwise order around
-- -- c. Split the list into points before p in the order, and points after the
-- -- order.
-- splitCCW c p []         = ([],[])
-- splitCCW c p (s:q:rest) = case ccwCmpAround c s q of
--                             LT -> -- there

--                               -- q occurs before p


-- -- insertCCW c p (s:pts =
