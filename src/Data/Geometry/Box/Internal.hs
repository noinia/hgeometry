{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DeriveFunctor  #-}
module Data.Geometry.Box.Internal where

import           Control.Lens
import           Data.Bifunctor
import           Data.Ext
import           Frames.CoRec (asA)
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector (Vector, Arity, Index',C(..))
import qualified Data.Geometry.Vector as V
import qualified Data.List.NonEmpty as NE
import           Data.Proxy
import qualified Data.Range as R
import           Data.Semigroup
import qualified Data.Semigroup.Foldable as F
import qualified Data.Vector.Fixed as FV
import           GHC.TypeLits

--------------------------------------------------------------------------------
-- * d-dimensional boxes

data Box d p r = Box { _minP :: Min (Point d r) :+ p
                     , _maxP :: Max (Point d r) :+ p
                     }
makeLenses ''Box

-- | Given the point with the lowest coordinates and the point with highest
-- coordinates, create a box.
fromCornerPoints          :: Point d r :+ p -> Point d r :+ p -> Box d p r
fromCornerPoints low high = Box (low&core %~ Min) (high&core %~ Max)

-- | Build a d dimensional Box given d ranges.
fromExtent    :: Arity d => Vector d (R.Range r) -> Box d () r
fromExtent rs = Box (Min (Point $ fmap (^.R.lower.R.unEndPoint) rs) :+ mempty)
                    (Max (Point $ fmap (^.R.upper.R.unEndPoint) rs) :+ mempty)


-- | Center of the box
centerPoint   :: (Arity d, Fractional r) => Box d p r -> Point d r
centerPoint b = Point $ w V.^/ 2
  where w = b^.minP.core.to getMin.vector V.^+^ b^.maxP.core.to getMax.vector



deriving instance (Show r, Show p, Arity d) => Show (Box d p r)
deriving instance (Eq r, Eq p, Arity d)     => Eq   (Box d p r)
deriving instance (Ord r, Ord p, Arity d)   => Ord  (Box d p r)

instance (Arity d, Ord r, Semigroup p) => Semigroup (Box d p r) where
  (Box mi ma) <> (Box mi' ma') = Box (mi <> mi') (ma <> ma')

type instance IntersectionOf (Box d p r) (Box d p r) = '[ NoIntersection, Box d () r]

instance (Ord r, Arity d) => (Box d p r) `IsIntersectableWith` (Box d p r) where
  nonEmptyIntersection = defaultNonEmptyIntersection

  box `intersect` box' = f . sequence $ FV.zipWith intersect' (extent box) (extent box')
    where
      f = maybe (coRec NoIntersection) (coRec . fromExtent)
      r `intersect'` s = asA (Proxy :: Proxy (R.Range r)) $ r `intersect` s



-- -- In principle this should also just work for Boxes in higher dimensions. It is just
-- -- that we need a better way to compute their corners
-- instance (Num r, Ord r) => (Rectangle p r) `IsIntersectableWith` (Rectangle p r) where

--   nonEmptyIntersection = defaultNonEmptyIntersection

--   box@(Box a b) `intersect` box'@(Box c d)
--       |    box  `containsACornerOf` box'
--         || box' `containsACornerOf` box = coRec $ Box (mi :+ ()) (ma :+ ())
--       | otherwise                       = coRec NoIntersection
--     where

--       mi = (a^.core) `max` (c^.core)
--       ma = (b^.core) `min` (d^.core)

--       bx `containsACornerOf` bx' = let (a',b',c',d') = corners bx'
--                                    in any (\(p :+ _) -> p `inBox` bx) [a',b',c',d']


type instance IntersectionOf (Point d r) (Box d p r) = '[ NoIntersection, Point d r]

instance (Arity d, Ord r) => (Point d r) `IsIntersectableWith` (Box d p r) where
  nonEmptyIntersection = defaultNonEmptyIntersection
  p `intersect` b
    | not $ p `inBox` b = coRec NoIntersection
    | otherwise         = coRec p


instance PointFunctor (Box d p) where
  pmap f (Box mi ma) = Box (first (fmap f) mi) (first (fmap f) ma)


instance (Num r, AlwaysTruePFT d) => IsTransformable (Box d p r) where
  -- Note that this does not guarantee the box is still a proper box Only use
  -- this to do translations and scalings. Other transformations may produce
  -- unexpected results.
  transformBy = transformPointFunctor


type instance Dimension (Box d p r) = d
type instance NumType   (Box d p r) = r

--------------------------------------------------------------------------------0
-- * Functions on d-dimensonal boxes

minPoint :: Box d p r -> Point d r :+ p
minPoint b = let (Min p :+ e) = b^.minP in p :+ e

maxPoint :: Box d p r -> Point d r :+ p
maxPoint b = let (Max p :+ e) = b^.maxP in p :+ e

-- | Check if a point lies a box
--
-- >>> origin `inBox` (boundingBoxList' [point3 1 2 3, point3 10 20 30] :: Box 3 () Int)
-- False
-- >>> origin `inBox` (boundingBoxList' [point3 (-1) (-2) (-3), point3 10 20 30] :: Box 3 () Int)
-- True
inBox :: (Arity d, Ord r) => Point d r -> Box d p r -> Bool
p `inBox` b = FV.and . FV.zipWith R.inRange (toVec p) . extent $ b

-- | Get a vector with the extent of the box in each dimension. Note that the
-- resulting vector is 0 indexed whereas one would normally count dimensions
-- starting at zero.
--
-- >>> extent (boundingBoxList' [point3 1 2 3, point3 10 20 30] :: Box 3 () Int)
-- Vector3 [Range {_lower = Closed 1, _upper = Closed 10},Range {_lower = Closed 2, _upper = Closed 20},Range {_lower = Closed 3, _upper = Closed 30}]
extent                                 :: Arity d
                                       => Box d p r -> Vector d (R.Range r)
extent (Box (Min a :+ _) (Max b :+ _)) = FV.zipWith R.ClosedRange (toVec a) (toVec b)

-- | Get the size of the box (in all dimensions). Note that the resulting vector is 0 indexed
-- whereas one would normally count dimensions starting at zero.
--
-- >>> size (boundingBoxList' [origin, point3 1 2 3] :: Box 3 () Int)
-- Vector3 [1,2,3]
size :: (Arity d, Num r) => Box d p r -> Vector d r
size = fmap R.width . extent

-- | Given a dimension, get the width of the box in that dimension. Dimensions are 1 indexed.
--
-- >>> widthIn (C :: C 1) (boundingBoxList' [origin, point3 1 2 3] :: Box 3 () Int)
-- 1
-- >>> widthIn (C :: C 3) (boundingBoxList' [origin, point3 1 2 3] :: Box 3 () Int)
-- 3
widthIn   :: forall proxy p i d r. (Arity d, Num r, Index' (i-1) d) => proxy i -> Box d p r -> r
widthIn _ = view (V.element (C :: C (i - 1))) . size


-- | Same as 'widthIn' but with a runtime int instead of a static dimension.
--
-- >>> widthIn' 1 (boundingBoxList' [origin, point3 1 2 3] :: Box 3 () Int)
-- Just 1
-- >>> widthIn' 3 (boundingBoxList' [origin, point3 1 2 3] :: Box 3 () Int)
-- Just 3
-- >>> widthIn' 10 (boundingBoxList' [origin, point3 1 2 3] :: Box 3 () Int)
-- Nothing
widthIn'   :: (Arity d, KnownNat d, Num r) => Int -> Box d p r -> Maybe r
widthIn' i = preview (V.element' (i-1)) . size


----------------------------------------
-- * Rectangles, aka 2-dimensional boxes

type Rectangle = Box 2

-- >>> width (boundingBoxList' [origin, point2 1 2] :: Rectangle () Int)
-- 1
-- >>> width (boundingBoxList' [origin] :: Rectangle () Int)
-- 0
width :: Num r => Rectangle p r -> r
width = widthIn (C :: C 1)

-- >>> height (boundingBoxList' [origin, point2 1 2] :: Rectangle () Int)
-- 2
-- >>> height (boundingBoxList' [origin] :: Rectangle () Int)
-- 0
height :: Num r => Rectangle p r -> r
height = widthIn (C :: C 2)


-- | Get the corners of a rectangle, the order is:
-- (TopLeft, TopRight, BottomRight, BottomLeft).
-- The extra values in the Top points are taken from the Top point,
-- the extra values in the Bottom points are taken from the Bottom point
corners :: Num r => Rectangle p r -> ( Point 2 r :+ p
                                     , Point 2 r :+ p
                                     , Point 2 r :+ p
                                     , Point 2 r :+ p
                                     )
corners r     = let w = width r
                    p = (_maxP r)&core %~ getMax
                    q = (_minP r)&core %~ getMin
                in ( p&core.xCoord %~ (subtract w)
                   , p
                   , q&core.xCoord %~ (+ w)
                   , q
                   )

--------------------------------------------------------------------------------
-- * Constructing bounding boxes

class IsBoxable g where
  boundingBox :: Ord (NumType g) => g -> Box (Dimension g) () (NumType g)


boundingBoxList :: (IsBoxable g, F.Foldable1 c, Ord (NumType g), Arity (Dimension g))
                => c g -> Box (Dimension g) () (NumType g)
boundingBoxList = F.foldMap1 boundingBox


-- | Unsafe version of boundingBoxList, that does not check if the list is non-empty
boundingBoxList' :: (IsBoxable g, Ord (NumType g), Arity (Dimension g))
                 => [g] -> Box (Dimension g) () (NumType g)
boundingBoxList' = boundingBoxList . NE.fromList

----------------------------------------

instance IsBoxable (Point d r) where
  boundingBox p = Box (ext $ Min p) (ext $ Max p)

instance IsBoxable (Box d p r) where
  boundingBox (Box m m') = Box (m&extra .~ ()) (m'&extra .~ ())
