{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE InstanceSigs  #-}
--------------------------------------------------------------------------------
-- |
-- Module      :  Data.Geometry.Box.Internal
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Orthogonal \(d\)-dimensiontal boxes (e.g. rectangles)
--
--------------------------------------------------------------------------------
module Data.Geometry.Box.Internal where

import           Control.DeepSeq
import           Control.Lens
import           Data.Bifoldable
import           Data.Bifunctor
import           Data.Bitraversable
import           Data.Ext
import qualified Data.Foldable as F
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import           Data.Geometry.Vector
import qualified Data.Geometry.Vector as V
import qualified Data.List.NonEmpty as NE
import qualified Data.Range as R
import qualified Data.Semigroup.Foldable as F
import qualified Data.Vector.Fixed as FV
import           Data.Vinyl.CoRec (asA)
import           GHC.Generics (Generic)
import           GHC.TypeLits
import           Test.QuickCheck (Arbitrary(..))

--------------------------------------------------------------------------------

-- | Coordinate wize minimum
newtype CWMin a = CWMin { _cwMin :: a }
                deriving (Show,Eq,Ord,Functor,Foldable,Traversable,Generic,NFData)
makeLenses ''CWMin

instance (Arity d, Ord r) => Semigroup (CWMin (Point d r)) where
  (CWMin p) <> (CWMin q) = CWMin . Point $ FV.zipWith min (p^.vector) (q^.vector)

-- | Coordinate wize maximum
newtype CWMax a = CWMax { _cwMax :: a }
                deriving (Show,Eq,Ord,Functor,Foldable,Traversable,Generic,NFData)
makeLenses ''CWMax

instance (Arity d, Ord r) => Semigroup (CWMax (Point d r)) where
  (CWMax p) <> (CWMax q) = CWMax . Point $ FV.zipWith max (p^.vector) (q^.vector)


--------------------------------------------------------------------------------
-- * d-dimensional boxes


data Box d p r = Box { _minP :: !(CWMin (Point d r) :+ p)
                     , _maxP :: !(CWMax (Point d r) :+ p)
                     } deriving Generic
makeLenses ''Box





-- | Given the point with the lowest coordinates and the point with highest
-- coordinates, create a box.
box          :: Point d r :+ p -> Point d r :+ p -> Box d p r
box low high = Box (low&core %~ CWMin) (high&core %~ CWMax)

-- | grows the box by x on all sides
grow     :: (Num r, Arity d) => r -> Box d p r -> Box d p r
grow x b = let v = V.replicate x
           in b&minP.core.cwMin %~ (.-^ v)
               &maxP.core.cwMax %~ (.+^ v)

-- | Build a d dimensional Box given d ranges.
fromExtent    :: Arity d => Vector d (R.Range r) -> Box d () r
fromExtent rs = Box (CWMin (Point $ fmap (^.R.lower.R.unEndPoint) rs) :+ mempty)
                    (CWMax (Point $ fmap (^.R.upper.R.unEndPoint) rs) :+ mempty)


-- | Given a center point and a vector specifying the box width's, construct a box.
fromCenter      :: (Arity d, Fractional r) => Point d r -> Vector d r -> Box d () r
fromCenter c ws = let f x r = R.ClosedRange (x-r) (x+r)
                  in fromExtent $ FV.zipWith f (toVec c) ((/2) <$> ws)


-- | Center of the box
centerPoint   :: (Arity d, Fractional r) => Box d p r -> Point d r
centerPoint b = Point $ w V.^/ 2
  where w = b^.minP.core.cwMin.vector V.^+^ b^.maxP.core.cwMax.vector



deriving instance (Show r, Show p, Arity d) => Show (Box d p r)
deriving instance (Eq r, Eq p, Arity d)     => Eq   (Box d p r)
deriving instance (Ord r, Ord p, Arity d)   => Ord  (Box d p r)

instance (Arity d, Ord r, Semigroup p) => Semigroup (Box d p r) where
  (Box mi ma) <> (Box mi' ma') = Box (mi <> mi') (ma <> ma')

type instance IntersectionOf (Box d p r) (Box d q r) = '[ NoIntersection, Box d () r]

instance (Ord r, Arity d) => (Box d p r) `IsIntersectableWith` (Box d q r) where
  nonEmptyIntersection = defaultNonEmptyIntersection

  bx `intersect` bx' = f . sequence $ FV.zipWith intersect' (extent bx) (extent bx')
    where
      f = maybe (coRec NoIntersection) (coRec . fromExtent)
      r `intersect'` s = asA @(R.Range r) $ r `intersect` s

instance Arity d => Bifunctor (Box d) where
  bimap = bimapDefault
instance Arity d => Bifoldable (Box d) where
  bifoldMap = bifoldMapDefault
instance Arity d => Bitraversable (Box d) where
  bitraverse f g (Box mi ma) = Box <$> bitraverse (tr g) f mi <*> bitraverse (tr g) f ma
    where
      tr    :: (Traversable t, Applicative f) => (r -> f s) -> t (Point d r) -> f (t (Point d s))
      tr g' = traverse $ traverse g'

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


instance (Fractional r, Arity d, Arity (d + 1))
         => IsTransformable (Box d p r) where
  -- Note that this does not guarantee the box is still a proper box Only use
  -- this to do translations and scalings. Other transformations may produce
  -- unexpected results.
  transformBy = transformPointFunctor


instance (Arbitrary r, Arity d, Ord r) => Arbitrary (Box d () r) where
  arbitrary = (\p (q :: Point d r) -> boundingBoxList' [p,q]) <$> arbitrary <*> arbitrary

type instance Dimension (Box d p r) = d
type instance NumType   (Box d p r) = r

--------------------------------------------------------------------------------0
-- * Functions on d-dimensonal boxes

minPoint :: Box d p r -> Point d r :+ p
minPoint b = let (CWMin p :+ e) = b^.minP in p :+ e

maxPoint :: Box d p r -> Point d r :+ p
maxPoint b = let (CWMax p :+ e) = b^.maxP in p :+ e

-- | Check if a point lies a box
--
-- >>> origin `inBox` (boundingBoxList' [Point3 1 2 3, Point3 10 20 30] :: Box 3 () Int)
-- False
-- >>> origin `inBox` (boundingBoxList' [Point3 (-1) (-2) (-3), Point3 10 20 30] :: Box 3 () Int)
-- True
inBox :: (Arity d, Ord r) => Point d r -> Box d p r -> Bool
p `inBox` b = FV.and . FV.zipWith R.inRange (toVec p) . extent $ b

-- | Get a vector with the extent of the box in each dimension. Note that the
-- resulting vector is 0 indexed whereas one would normally count dimensions
-- starting at zero.
--
-- >>> extent (boundingBoxList' [Point3 1 2 3, Point3 10 20 30] :: Box 3 () Int)
-- Vector3 [Range (Closed 1) (Closed 10),Range (Closed 2) (Closed 20),Range (Closed 3) (Closed 30)]
extent                                 :: Arity d
                                       => Box d p r -> Vector d (R.Range r)
extent (Box (CWMin a :+ _) (CWMax b :+ _)) = FV.zipWith R.ClosedRange (toVec a) (toVec b)

-- | Get the size of the box (in all dimensions). Note that the resulting vector is 0 indexed
-- whereas one would normally count dimensions starting at zero.
--
-- >>> size (boundingBoxList' [origin, Point3 1 2 3] :: Box 3 () Int)
-- Vector3 [1,2,3]
size :: (Arity d, Num r) => Box d p r -> Vector d r
size = fmap R.width . extent

-- | Given a dimension, get the width of the box in that dimension. Dimensions are 1 indexed.
--
-- >>> widthIn (C :: C 1) (boundingBoxList' [origin, Point3 1 2 3] :: Box 3 () Int)
-- 1
-- >>> widthIn (C :: C 3) (boundingBoxList' [origin, Point3 1 2 3] :: Box 3 () Int)
-- 3
widthIn   :: forall proxy p i d r. (Arity d, Arity (i - 1), Num r, ((i-1)+1) <= d)
          => proxy i -> Box d p r -> r
widthIn _ = view (V.element (C :: C (i - 1))) . size


-- | Same as 'widthIn' but with a runtime int instead of a static dimension.
--
-- >>> widthIn' 1 (boundingBoxList' [origin, Point3 1 2 3] :: Box 3 () Int)
-- Just 1
-- >>> widthIn' 3 (boundingBoxList' [origin, Point3 1 2 3] :: Box 3 () Int)
-- Just 3
-- >>> widthIn' 10 (boundingBoxList' [origin, Point3 1 2 3] :: Box 3 () Int)
-- Nothing
widthIn'   :: (Arity d, Num r) => Int -> Box d p r -> Maybe r
widthIn' i = preview (V.element' (i-1)) . size


----------------------------------------
-- * Rectangles, aka 2-dimensional boxes

type Rectangle = Box 2

-- >>> width (boundingBoxList' [origin, Point2 1 2] :: Rectangle () Int)
-- 1
-- >>> width (boundingBoxList' [origin] :: Rectangle () Int)
-- 0
width :: Num r => Rectangle p r -> r
width = widthIn (C :: C 1)

-- >>> height (boundingBoxList' [origin, Point2 1 2] :: Rectangle () Int)
-- 2
-- >>> height (boundingBoxList' [origin] :: Rectangle () Int)
-- 0
height :: Num r => Rectangle p r -> r
height = widthIn (C :: C 2)


--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- * Constructing bounding boxes

class IsBoxable g where
  boundingBox :: Ord (NumType g) => g -> Box (Dimension g) () (NumType g)


boundingBoxList :: (IsBoxable g, F.Foldable1 c, Ord (NumType g), Arity (Dimension g))
                => c g -> Box (Dimension g) () (NumType g)
boundingBoxList = F.foldMap1 boundingBox


-- | Unsafe version of boundingBoxList, that does not check if the list is non-empty
boundingBoxList' :: (IsBoxable g, Foldable c, Ord (NumType g), Arity (Dimension g))
                 => c g -> Box (Dimension g) () (NumType g)
boundingBoxList' = boundingBoxList . NE.fromList . F.toList

----------------------------------------

instance IsBoxable (Point d r) where
  boundingBox p = Box (ext $ CWMin p) (ext $ CWMax p)

instance IsBoxable (Box d p r) where
  boundingBox (Box m m') = Box (m&extra .~ ()) (m'&extra .~ ())

instance IsBoxable c => IsBoxable (c :+ e) where
  boundingBox = boundingBox . view core
