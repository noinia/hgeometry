{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.QuadTree.Cell where

import Control.Lens (makeLenses, (^.),(&),(%~),ix, to)
import Data.Bifunctor
import Data.Ext
import Data.Geometry.Box
import Data.Geometry.Directions
import Data.Geometry.LineSegment
import Data.Geometry.Point
import Data.Geometry.Properties
import Data.Geometry.QuadTree.Quadrants
import Data.Geometry.Vector
import Data.Intersection

--------------------------------------------------------------------------------

-- | side lengths will be 2^i for some integer i
type WidthIndex = Int

-- | A Cell corresponding to a node in the QuadTree
data Cell r = Cell { _cellWidthIndex :: {-# UNPACK #-} !WidthIndex
                   , _lowerLeft      ::                !(Point 2 r)
                   } deriving (Show,Eq,Functor,Foldable,Traversable)
makeLenses ''Cell

-- | Computes a cell that contains the given rectangle
fitsRectangle   :: (RealFrac r, Ord r) => Rectangle p r -> Cell r
fitsRectangle b = Cell w ((b^.to minPoint.core) .-^ Vector2 1 1)
  where
    w = lg' . ceiling . (1+) . maximum . size $ b

    -- "approximate log" that over approximates by a factor of at most two.
    lg'   :: Integer -> WidthIndex
    lg' n = go 1
      where
        go i | floor (pow i) <= n = go (i+1) -- note that the floor does not really do anything
                                             -- since i is integral and >= 1.
             | otherwise  = i

type instance Dimension (Cell r) = 2
type instance NumType   (Cell r) = r

type instance IntersectionOf (Point 2 r) (Cell r) = '[ NoIntersection, Point 2 r]

instance (Ord r, Fractional r) => (Point 2 r) `IsIntersectableWith` (Cell r) where
  nonEmptyIntersection = defaultNonEmptyIntersection
  p `intersect` c = p `intersect` toBox c

pow   :: Fractional r => WidthIndex -> r
pow i = case i `compare` 0 of
          LT -> 1 / (2 ^ (-1*i))
          EQ -> 1
          GT -> 2 ^ i

cellWidth            :: Fractional r => Cell r -> r
cellWidth (Cell w _) = pow w

toBox            :: Fractional r => Cell r -> Box 2 () r
toBox (Cell w p) = box (ext $ p) (ext $ p .+^ Vector2 (pow w) (pow w))

inCell            :: (Fractional r, Ord r) => Point 2 r :+ p -> Cell r -> Bool
inCell (p :+ _) c = p `inBox` toBox c

cellCorners :: Fractional r => Cell r -> Quadrants (Point 2 r)
cellCorners = fmap (^.core) . corners . toBox

-- | Sides are open
cellSides :: Fractional r => Cell r -> Sides (LineSegment 2 () r)
cellSides = fmap (\(ClosedLineSegment p q) -> OpenLineSegment p q) . sides . toBox

splitCell            :: (Num r, Fractional r) => Cell r -> Quadrants (Cell r)
splitCell (Cell w p) = Quadrants (Cell r $ f 0 rr)
                                 (Cell r $ f rr rr)
                                 (Cell r $ f rr 0)
                                 (Cell r p)
  where
    r     = w - 1
    rr    = pow r
    f x y = p .+^ Vector2 x y


midPoint            :: Fractional r => Cell r -> Point 2 r
midPoint (Cell w p) = let rr = pow (w - 1) in p .+^ Vector2 rr rr


--------------------------------------------------------------------------------

-- | Partitions the points into quadrants. See 'quadrantOf' for the
-- precise rules.
partitionPoints   :: (Fractional r, Ord r)
                  => Cell r -> [Point 2 r :+ p] -> Quadrants [Point 2 r :+ p]
partitionPoints c = foldMap (\p -> let q = quadrantOf (p^.core) c in mempty&ix q %~ (p:))

-- | Computes the quadrant of the cell corresponding to the current
-- point. Note that we decide the quadrant solely based on the
-- midpoint. If the query point lies outside the cell, it is still
-- assigned a quadrant.
--
-- - The northEast quadrants includes its bottom and left side
-- - The southEast quadrant  includes its            left side
-- - The northWest quadrant  includes its bottom          side
-- - The southWest quadrants does not include any of its sides.
--
--
-- >>> quadrantOf (Point2 9 9) (Cell 4 origin)
-- NorthEast
-- >>> quadrantOf (Point2 8 9) (Cell 4 origin)
-- NorthEast
-- >>> quadrantOf (Point2 8 8) (Cell 4 origin)
-- NorthEast
-- >>> quadrantOf (Point2 8 7) (Cell 4 origin)
-- SouthEast
-- >>> quadrantOf (Point2 4 7) (Cell 4 origin)
-- SouthWest
-- >>> quadrantOf (Point2 4 10) (Cell 4 origin)
-- NorthWest
-- >>> quadrantOf (Point2 4 40) (Cell 4 origin)
-- NorthEast
-- >>> quadrantOf (Point2 4 40) (Cell 4 origin)
-- NorthWest
quadrantOf     :: forall r. (Fractional r, Ord r)
               => Point 2 r -> Cell r -> InterCardinalDirection
quadrantOf q c = let m = midPoint c
                 in case (q^.xCoord < m^.xCoord, q^.yCoord < m^.yCoord) of
                      (False,False) -> NorthEast
                      (False,True)  -> SouthEast
                      (True,False)  -> NorthWest
                      (True,True)   -> SouthWest



-- | Given two cells c and me, compute on which side of `me` the cell
-- `c` is.
--
-- pre: c and me are non-overlapping
relationTo        :: (Fractional r, Ord r)
                  => (p :+ Cell r) -> Cell r -> Sides (Maybe (p :+ Cell r))
c `relationTo` me = f <$> Sides b l t r <*> cellSides me
  where
    Sides t r b l = cellSides (c^.extra)
    f e e' | e `intersects` e' = Just c
           | otherwise         = Nothing
