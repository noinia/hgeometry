{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Point
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- \(d\)-dimensional points.
--
--------------------------------------------------------------------------------
module Data.Geometry.Point( Point(..)
                          , origin, vector
                          , pointFromList

                          , coord , unsafeCoord

                          , projectPoint

                          , pattern Point2
                          , pattern Point3
                          , xCoord, yCoord, zCoord

                          , PointFunctor(..)

                          , CCW(..), ccw, ccw'

                          , ccwCmpAround, cwCmpAround, ccwCmpAroundWith, cwCmpAroundWith
                          , sortAround, insertIntoCyclicOrder

                          , Quadrant(..), quadrantWith, quadrant, partitionIntoQuadrants

                          , cmpByDistanceTo

                          , squaredEuclideanDist, euclideanDist
                          ) where

import           Control.DeepSeq
import           Control.Lens
import           Data.Aeson
import qualified Data.CircularList as C
import qualified Data.CircularList.Util as CU
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Properties
import           Data.Geometry.Vector
import qualified Data.Geometry.Vector as Vec
import           Data.Hashable
import qualified Data.List as L
import           Data.Ord (comparing)
import           Data.Proxy
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           System.Random (Random(..))
import           Test.QuickCheck (Arbitrary)
import           Text.ParserCombinators.ReadP (ReadP, string,pfail)
import           Text.ParserCombinators.ReadPrec (lift)
import           Text.Read (Read(..),readListPrecDefault, readPrec_to_P,minPrec)


--------------------------------------------------------------------------------
-- $setup
-- >>> :{
-- let myVector :: Vector 3 Int
--     myVector = Vector3 1 2 3
--     myPoint = Point myVector
-- :}


--------------------------------------------------------------------------------
-- * A d-dimensional Point

-- | A d-dimensional point.
newtype Point d r = Point { toVec :: Vector d r } deriving (Generic)

instance (Show r, Arity d) => Show (Point d r) where
  show (Point v) = mconcat [ "Point", show $ F.length v , " "
                           , show $ F.toList v
                           ]
instance (Read r, Arity d) => Read (Point d r) where
  readPrec     = lift readPt
  readListPrec = readListPrecDefault

readPt :: forall d r. (Arity d, Read r) => ReadP (Point d r)
readPt = do let d = natVal (Proxy :: Proxy d)
            _  <- string $ "Point" <> show d <> " "
            rs <- readPrec_to_P readPrec minPrec
            case pointFromList rs of
              Just p -> pure p
              _      -> pfail

deriving instance (Eq r, Arity d)        => Eq (Point d r)
deriving instance (Ord r, Arity d)       => Ord (Point d r)
deriving instance Arity d                => Functor (Point d)
deriving instance Arity d                => Foldable (Point d)
deriving instance Arity d                => Traversable (Point d)
deriving instance (Arity d, NFData r)    => NFData (Point d r)
deriving instance (Arity d, Arbitrary r) => Arbitrary (Point d r)
deriving instance (Arity d, Hashable r)  => Hashable (Point d r)
deriving instance (Arity d, Random r)    => Random (Point d r)


type instance NumType (Point d r) = r
type instance Dimension (Point d r) = d

instance Arity d =>  Affine (Point d) where
  type Diff (Point d) = Vector d

  p .-. q = toVec p ^-^ toVec q
  p .+^ v = Point $ toVec p ^+^ v

instance (FromJSON r, Arity d, KnownNat d) => FromJSON (Point d r) where
  parseJSON = fmap Point . parseJSON

instance (ToJSON r, Arity d) => ToJSON (Point d r) where
  toJSON     = toJSON     . toVec
  toEncoding = toEncoding . toVec

-- | Point representing the origin in d dimensions
--
-- >>> origin :: Point 4 Int
-- Point4 [0,0,0,0]
origin :: (Arity d, Num r) => Point d r
origin = Point $ pure 0


-- ** Accessing points

-- | Lens to access the vector corresponding to this point.
--
-- >>> (Point3 1 2 3) ^. vector
-- Vector3 [1,2,3]
-- >>> origin & vector .~ Vector3 1 2 3
-- Point3 [1,2,3]
vector :: Lens' (Point d r) (Vector d r)
vector = lens toVec (const Point)
{-# INLINABLE vector #-}

-- | Get the coordinate in a given dimension. This operation is unsafe in the
-- sense that no bounds are checked. Consider using `coord` instead.
--
--
-- >>> Point3 1 2 3 ^. unsafeCoord 2
-- 2
unsafeCoord   :: Arity d => Int -> Lens' (Point d r) r
unsafeCoord i = vector . singular (ix (i-1))
                -- Points are 1 indexed, vectors are 0 indexed
{-# INLINABLE unsafeCoord #-}

-- | Get the coordinate in a given dimension
--
-- >>> Point3 1 2 3 ^. coord (C :: C 2)
-- 2
-- >>> Point3 1 2 3 & coord (C :: C 1) .~ 10
-- Point3 [10,2,3]
-- >>> Point3 1 2 3 & coord (C :: C 3) %~ (+1)
-- Point3 [1,2,4]
coord   :: forall proxy i d r. (1 <= i, i <= d, ((i - 1) + 1) ~ i
                               , Arity (i - 1), Arity d
                               ) => proxy i -> Lens' (Point d r) r
coord _ = vector . Vec.element (Proxy :: Proxy (i-1))
{-# INLINABLE coord #-}


-- somehow these rules don't fire
-- {-# SPECIALIZE coord :: C 1 -> Lens' (Point 2 r) r#-}
-- {-# SPECIALIZE coord :: C 2 -> Lens' (Point 2 r) r#-}


-- | Constructs a point from a list of coordinates
--
-- >>> pointFromList [1,2,3] :: Maybe (Point 3 Int)
-- Just Point3 [1,2,3]
pointFromList :: Arity d => [r] -> Maybe (Point d r)
pointFromList = fmap Point . Vec.vectorFromList


-- | Project a point down into a lower dimension.
projectPoint :: (Arity i, Arity d, i <= d) => Point d r -> Point i r
projectPoint = Point . prefix . toVec

--------------------------------------------------------------------------------
-- * Convenience functions to construct 2 and 3 dimensional points


-- | We provide pattern synonyms Point2 and Point3 for 2 and 3 dimensional points. i.e.
-- we can write:
--
-- >>> :{
--   let
--     f              :: Point 2 r -> r
--     f (Point2 x y) = x
--   in f (Point2 1 2)
-- :}
-- 1
--
-- if we want.
pattern Point2       :: r -> r -> Point 2 r
pattern Point2 x y = Point (Vector2 x y)
{-# COMPLETE Point2 #-}

-- | Similarly, we can write:
--
-- >>> :{
--   let
--     g                :: Point 3 r -> r
--     g (Point3 x y z) = z
--   in g myPoint
-- :}
-- 3
pattern Point3       :: r -> r -> r -> Point 3 r
pattern Point3 x y z = (Point (Vector3 x y z))
{-# COMPLETE Point3 #-}

-- | Shorthand to access the first coordinate C 1
--
-- >>> Point3 1 2 3 ^. xCoord
-- 1
-- >>> Point2 1 2 & xCoord .~ 10
-- Point2 [10,2]
xCoord :: (1 <= d, Arity d) => Lens' (Point d r) r
xCoord = coord (C :: C 1)
{-# INLINABLE xCoord #-}

-- | Shorthand to access the second coordinate C 2
--
-- >>> Point2 1 2 ^. yCoord
-- 2
-- >>> Point3 1 2 3 & yCoord %~ (+1)
-- Point3 [1,3,3]
yCoord :: (2 <= d, Arity d) => Lens' (Point d r) r
yCoord = coord (C :: C 2)
{-# INLINABLE yCoord #-}

-- | Shorthand to access the third coordinate C 3
--
-- >>> Point3 1 2 3 ^. zCoord
-- 3
-- >>> Point3 1 2 3 & zCoord %~ (+1)
-- Point3 [1,2,4]
zCoord :: (3 <= d, Arity d) => Lens' (Point d r) r
zCoord = coord (C :: C 3)
{-# INLINABLE zCoord #-}


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

-- | Given three points p q and r determine the orientation when going from p to r via q.
ccw' :: (Ord r, Num r) => Point 2 r :+ a -> Point 2 r :+ b -> Point 2 r :+ c -> CCW
ccw' p q r = ccw (p^.core) (q^.core) (r^.core)

-- | Sort the points arround the given point p in counter clockwise order with
-- respect to the rightward horizontal ray starting from p.  If two points q
-- and r are colinear with p, the closest one to p is reported first.
-- running time: O(n log n)
sortAround   :: (Ord r, Num r)
             => Point 2 r :+ q -> [Point 2 r :+ p] -> [Point 2 r :+ p]
sortAround c = L.sortBy (ccwCmpAround c <> cmpByDistanceTo c)


-- | Quadrants of two dimensional points. in CCW order
data Quadrant = TopRight | TopLeft | BottomLeft | BottomRight
              deriving (Show,Read,Eq,Ord,Enum,Bounded)

-- | Quadrants around point c; quadrants are closed on their "previous"
-- boundary (i..e the boundary with the previous quadrant in the CCW order),
-- open on next boundary. The origin itself is assigned the topRight quadrant
quadrantWith                   :: (Ord r, 1 <= d, 2 <= d, Arity d)
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
quadrant :: (Ord r, Num r, 1 <= d, 2 <= d, Arity d) => Point d r :+ p -> Quadrant
quadrant = quadrantWith (ext origin)

-- | Given a center point c, and a set of points, partition the points into
-- quadrants around c (based on their x and y coordinates). The quadrants are
-- reported in the order topLeft, topRight, bottomLeft, bottomRight. The points
-- are in the same order as they were in the original input lists.
-- Points with the same x-or y coordinate as p, are "rounded" to above.
partitionIntoQuadrants       :: (Ord r, 1 <= d, 2 <= d, Arity d)
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



-- | Given a zero vector z, a center c, and two points p and q,
-- compute the ccw ordering of p and q around c with this vector as zero
-- direction.
--
-- pre: the points p,q /= c
ccwCmpAroundWith                              :: (Ord r, Num r)
                                              => Vector 2 r
                                              -> Point 2 r :+ c
                                              -> Point 2 r :+ a -> Point 2 r :+ b
                                              -> Ordering
ccwCmpAroundWith z@(Vector2 zx zy) (c :+ _) (q :+ _) (r :+ _) =
    case (ccw c a q, ccw c a r) of
      (CCW,CCW)      -> cmp
      (CCW,CW)       -> LT
      (CCW,CoLinear) | onZero r  -> GT
                     | otherwise -> LT

      (CW, CCW)      -> GT
      (CW, CW)       -> cmp
      (CW, CoLinear) -> GT

      (CoLinear, CCW) | onZero q  -> LT
                      | otherwise -> GT

      (CoLinear, CW)      -> LT
      (CoLinear,CoLinear) -> case (onZero q, onZero r) of
                               (True, True)   -> EQ
                               (False, False) -> EQ
                               (True, False)  -> LT
                               (False, True)  -> GT
  where
    a = c .+^ z
    b = c .+^ Vector2 (-zy) zx
    -- b is on a perpendicular vector to z

    -- test if the point lies on the ray defined by z, starting in c
    onZero d = case ccw c b d of
                 CCW      -> False
                 CW       -> True
                 CoLinear -> True -- this shouldh appen only when you ask for c itself

    cmp = case ccw c q r of
            CCW      -> LT
            CW       -> GT
            CoLinear -> EQ

-- | Given a zero vector z, a center c, and two points p and q,
-- compute the cw ordering of p and q around c with this vector as zero
-- direction.
--
-- pre: the points p,q /= c
cwCmpAroundWith     :: (Ord r, Num r)
                    => Vector 2 r
                    -> Point 2 r :+ a
                    -> Point 2 r :+ b -> Point 2 r :+ c
                    -> Ordering
cwCmpAroundWith z c = flip (ccwCmpAroundWith z c)



-- | Compare by distance to the first argument
cmpByDistanceTo              :: (Ord r, Num r, Arity d)
                             => Point d r :+ c -> Point d r :+ p -> Point d r :+ q -> Ordering
cmpByDistanceTo (c :+ _) p q = comparing (squaredEuclideanDist c) (p^.core) (q^.core)


-- | Counter clockwise ordering of the points around c. Points are ordered with
-- respect to the positive x-axis.
ccwCmpAround :: (Num r, Ord r)
             => Point 2 r :+ qc -> Point 2 r :+ p -> Point 2 r :+ q -> Ordering
ccwCmpAround = ccwCmpAroundWith (Vector2 1 0)

-- | Clockwise ordering of the points around c. Points are ordered with
-- respect to the positive x-axis.
cwCmpAround :: (Num r, Ord r)
            => Point 2 r :+ qc -> Point 2 r :+ p -> Point 2 r :+ q -> Ordering
cwCmpAround = cwCmpAroundWith (Vector2 1 0)


-- | Given a center c, a new point p, and a list of points ps, sorted in
-- counter clockwise order around c. Insert p into the cyclic order. The focus
-- of the returned cyclic list is the new point p.
--
-- running time: O(n)
insertIntoCyclicOrder   :: (Ord r, Num r)
                        => Point 2 r :+ q -> Point 2 r :+ p
                        -> C.CList (Point 2 r :+ p) -> C.CList (Point 2 r :+ p)
insertIntoCyclicOrder c = CU.insertOrdBy (ccwCmpAround c <> cmpByDistanceTo c)


-- | Squared Euclidean distance between two points
squaredEuclideanDist :: (Num r, Arity d) => Point d r -> Point d r -> r
squaredEuclideanDist = qdA

-- | Euclidean distance between two points
euclideanDist :: (Floating r, Arity d) => Point d r -> Point d r -> r
euclideanDist = distanceA
