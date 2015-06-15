{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE DeriveFunctor  #-}
module Data.Geometry.Box(
                        -- * d-dimensional boxes
                          Box(..)

                        -- * Constructing bounding boxes
                        , IsBoxable(..)
                        , IsAlwaysTrueBoundingBox
                        , boundingBoxList

                        -- * Functions on d-dimensonal boxes
                        , minPoint, maxPoint
                        , extent, size, widthIn
                        , inBox

                        -- * Rectangles, aka 2-dimensional boxes
                        , Rectangle
                        , width , height
                        ) where

import           Control.Applicative
import           Control.Lens(Getter,to,(^.),view)
import           Data.Bifunctor
import           Data.Ext
import qualified Data.Foldable as F
import qualified Data.Range as R
import           Data.Geometry.Point
import           Data.Geometry.Properties
import           Data.Geometry.Transformation
import qualified Data.Geometry.Vector as V
import           Data.Geometry.Vector(Vector, Arity, Index',C(..))
import           Data.Maybe(catMaybes, maybe)
import           Data.Semigroup
import           Data.Vinyl
import           Frames.CoRec
import           Linear.Affine((.-.))

import qualified Data.Vector.Fixed                as FV

import           GHC.TypeLits
--------------------------------------------------------------------------------

data Box d p r = Empty
               | Box { _minP :: Min (Point d r) :+ p
                     , _maxP :: Max (Point d r) :+ p
                     }

deriving instance (Show r, Show p, Arity d) => Show (Box d p r)
deriving instance (Eq r, Eq p, Arity d)     => Eq   (Box d p r)
deriving instance (Ord r, Ord p, Arity d)   => Ord  (Box d p r)

instance (Arity d, Ord r, Semigroup p) => Semigroup (Box d p r) where
  Empty       <> b             = b
  b           <> Empty         = b
  (Box mi ma) <> (Box mi' ma') = Box (mi <> mi') (ma <> ma')


instance (Arity d, Ord r, Semigroup p) => Monoid (Box d p r) where
  mempty = Empty
  b `mappend` b' = b <> b'

type instance IntersectionOf (Box d p r) (Box d q r) = '[ Box d () r]


instance (Arity d, Ord r) => (Box d p r) `IsIntersectableWith` (Box d p r) where

  nonEmptyIntersection _ _ x = match x (H f :& RNil)
    where
      f Empty = True
      f _     = False

  bx@(Box a b) `intersect` by@(Box c d) = coRec $ Box (mi :+ ()) (ma :+ ())
    where
      mi = (a^.core) `max` (c^.core)
      ma = (b^.core) `min` (d^.core)


instance PointFunctor (Box d p) where
  pmap f (Box mi ma) = Box (first (fmap f) mi) (first (fmap f) ma)


instance (Num r, AlwaysTruePFT d) => IsTransformable (Box d p r) where
  -- Note that this does not guarantee the box is still a proper box Only use
  -- this to do translations and scalings. Other transformations may produce
  -- unexpected results.
  transformBy = transformPointFunctor


type instance Dimension (Box d p r) = d
type instance NumType   (Box d p r) = r

to'     :: (m -> Point d r) -> (Box d p r -> m :+ p) ->
           Getter (Box d p r) (Maybe (Point d r :+ p))
to' f g = to $ \x -> case x of
                  Empty -> Nothing
                  b     -> Just . first f . g $ b

minPoint :: Getter (Box d p r) (Maybe (Point d r :+ p))
minPoint = to' getMin _minP

maxPoint :: Getter (Box d p r) (Maybe (Point d r :+ p))
maxPoint = to' getMax _maxP

-- | Check if a point lies a box
--
-- >>> origin `inBox` (boundingBoxList [point3 1 2 3, point3 10 20 30] :: Box 3 () Int)
-- False
-- >>> origin `inBox` (boundingBoxList [point3 (-1) (-2) (-3), point3 10 20 30] :: Box 3 () Int)
-- True
inBox :: (Arity d, Ord r) => Point d r -> Box d p r -> Bool
p `inBox` b = maybe False f $ extent b
  where
    f = FV.and . FV.zipWith R.inRange (toVec p)



-- | Get a vector with the extent of the box in each dimension. Note that the
-- resulting vector is 0 indexed whereas one would normally count dimensions
-- starting at zero.
--
-- >>> extent (boundingBoxList [point3 1 2 3, point3 10 20 30] :: Box 3 () Int)
-- Just (Vector {_unV = fromList [Range {_lower = Closed 1, _upper = Closed 10},Range {_lower = Closed 2, _upper = Closed 20},Range {_lower = Closed 3, _upper = Closed 30}]})
extent                                 :: (Arity d)
                                       => Box d p r -> Maybe (Vector d (R.Range r))
extent Empty                           = Nothing
extent (Box (Min a :+ _) (Max b :+ _)) = Just $ FV.zipWith R.ClosedRange (toVec a) (toVec b)

-- | Get the size of the box (in all dimensions). Note that the resulting vector is 0 indexed
-- whereas one would normally count dimensions starting at zero.
--
-- >>> size (boundingBoxList [origin, point3 1 2 3] :: Box 3 () Int)
-- Vector {_unV = fromList [1,2,3]}
size :: (Arity d, Num r) => Box d p r -> Vector d r
size = maybe (pure 0) (fmap R.width) . extent

-- | Given a dimension, get the width of the box in that dimension. Dimensions are 1 indexed.
--
-- >>> widthIn (C :: C 1) (boundingBoxList [origin, point3 1 2 3] :: Box 3 () Int)
-- 1
-- >>> widthIn (C :: C 3) (boundingBoxList [origin, point3 1 2 3] :: Box 3 () Int)
-- 3
widthIn   :: forall proxy p i d r. (Arity d, Num r, Index' (i-1) d) => proxy i -> Box d p r -> r
widthIn _ = view (V.element (C :: C (i - 1))) . size

----------------------------------------

type Rectangle = Box 2

-- >>> width (boundingBoxList [origin, point2 1 2] :: Rectangle () Int)
-- 1
-- >>> width (boundingBoxList [origin] :: Rectangle () Int)
-- 0
width :: Num r => Rectangle p r -> r
width = widthIn (C :: C 1)

-- >>> height (boundingBoxList [origin, point2 1 2] :: Rectangle () Int)
-- 2
-- >>> height (boundingBoxList [origin] :: Rectangle () Int)
-- 0
height :: Num r => Rectangle p r -> r
height = widthIn (C :: C 2)


--------------------------------------------------------------------------------

class IsBoxable g where
  boundingBox :: (Monoid p, Semigroup p, Ord (NumType g))
              => g -> Box (Dimension g) p (NumType g)

type IsAlwaysTrueBoundingBox g p = (Semigroup p, Arity (Dimension g))


boundingBoxList :: (IsBoxable g, Monoid p, F.Foldable c, Ord (NumType g)
                   , IsAlwaysTrueBoundingBox g p
                   ) => c g -> Box (Dimension g) p (NumType g)
boundingBoxList = F.foldMap boundingBox

----------------------------------------

instance IsBoxable (Point d r) where
  boundingBox p = Box (Min p :+ mempty) (Max p :+ mempty)
