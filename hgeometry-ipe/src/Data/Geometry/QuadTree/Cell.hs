{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.QuadTree.Cell where

import           Control.Lens (makeLenses, (^.),(&),(%~),ix)
import           Data.Bifunctor
import           Data.Ext
import           Data.Geometry.Box
import           Data.Geometry.LineSegment
import           Data.Geometry.Point
import           Data.Geometry.QuadTree.Quadrants
import           Data.Geometry.Vector
import           Data.Intersection

--------------------------------------------------------------------------------

-- | side lengths will be 2^i for some integer i
type WidthIndex = Int

type R = Int

toR :: Num r => Int -> r
toR = fromInteger . toInteger


-- | A Cell corresponding to a node in the QuadTree
data Cell = Cell { _cellWidthIndex :: WidthIndex
                 , _lowerLeft      :: Point 2 Int
                 } deriving (Show,Eq)
makeLenses ''Cell


type instance IntersectionOf (Point 2 r) Cell = '[ NoIntersection, Point 2 r]

instance (Ord r, Num r) => (Point 2 r) `IsIntersectableWith` Cell where
  nonEmptyIntersection = defaultNonEmptyIntersection
  p `intersect` c = p `intersect` (second (toR @r) $ toBox c)

cellWidth            :: Cell -> Int
cellWidth (Cell w _) = 2 ^ w

toBox            :: Cell -> Box 2 () Int
toBox (Cell w p) = box (ext $ p) (ext $ p .+^ Vector2 (2^w) (2^w))

inCell            :: Point 2 R :+ p -> Cell -> Bool
inCell (p :+ _) c = p `inBox` (toBox c)

cellCorners :: Cell -> Quadrants (Point 2 R)
cellCorners = fmap (^.core) . corners . toBox

-- | Sides are open
cellSides :: Cell -> Sides (LineSegment 2 () R)
cellSides = fmap (\(ClosedLineSegment p q) -> OpenLineSegment p q) . sides . toBox

splitCell            :: Cell -> Quadrants Cell
splitCell (Cell w p) = Quadrants (Cell r $ f 0 rr)
                                 (Cell r $ f rr rr)
                                 (Cell r $ f rr 0)
                                 (Cell r p)
  where
    r     = w - 1
    rr    = 2 ^ r
    f x y = p .+^ Vector2 x y


midPoint            :: Cell -> Point 2 Int
midPoint (Cell w p) = let rr = 2 ^ (w - 1) in p .+^ Vector2 rr rr


--------------------------------------------------------------------------------

-- | Partitions the points into quadrants. See 'quadrantOf' for the
-- precise rules.
partitionPoints   :: (Num r, Ord r) => Cell -> [Point 2 r :+ p] -> Quadrants [Point 2 r :+ p]
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
quadrantOf     :: forall r. (Num r, Ord r)
               => Point 2 r -> Cell -> InterCardinalDirection
quadrantOf q c = let m = toR <$> midPoint c
                 in case (q^.xCoord < m^.xCoord, q^.yCoord < m^.yCoord) of
                      (False,False) -> NorthEast
                      (False,True)  -> SouthEast
                      (True,False)  -> NorthWest
                      (True,True)   -> SouthWest
