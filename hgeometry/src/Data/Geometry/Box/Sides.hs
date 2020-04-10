{-# LANGUAGE TemplateHaskell  #-}
module Data.Geometry.Box.Sides( Sides(Sides), north, east, south, west
                              , topSide, bottomSide, leftSide, rightSide
                              , sides, sides'

                              , sideDirections
                              ) where

import Data.Geometry.Directions
import Data.Geometry.Box.Internal
import Data.Geometry.Box.Corners
import Data.Geometry.LineSegment
import Data.Functor.Apply
import Data.Semigroup.Foldable.Class
import Data.Semigroup.Traversable.Class
import GHC.Generics (Generic)
import Control.Lens(makeLenses, Ixed(..), Index, IxValue)

--------------------------------------------------------------------------------

-- | The four sides of a rectangle
data Sides a = Sides { _north :: !a
                     , _east  :: !a
                     , _south :: !a
                     , _west  :: !a
                     } deriving (Show,Read,Eq,Generic,Ord,Foldable,Functor,Traversable)
makeLenses ''Sides

instance Applicative Sides where
  pure x = Sides x x x x
  (Sides f g h i) <*> (Sides a b c d) = Sides (f a) (g b) (h c) (i d)

instance Foldable1 Sides
instance Traversable1 Sides where
  traverse1 f (Sides a b c d) = Sides <$> f a <.> f b <.> f c <.> f d

instance Semigroup a => Semigroup (Sides a) where
  s <> s' = (<>) <$> s <*> s'
instance Monoid a => Monoid (Sides a) where
  mempty = pure mempty


type instance Index   (Sides a) = CardinalDirection
type instance IxValue (Sides a) = a

instance Ixed (Sides a) where
  ix = \case
    North -> north
    East  -> east
    South -> south
    West  -> west

-- | Constructs a Sides value that indicates the appropriate
-- direction.
sideDirections :: Sides CardinalDirection
sideDirections = Sides North East South West

--------------------------------------------------------------------------------

topSide :: Num r => Rectangle p r -> LineSegment 2 p r
topSide = (\(Corners l r _ _) -> ClosedLineSegment l r) . corners

-- | Oriented from *left to right*
bottomSide :: Num r => Rectangle p r -> LineSegment 2 p r
bottomSide = (\(Corners _ _ r l) -> ClosedLineSegment l r) . corners

--
leftSide  :: Num r => Rectangle p r -> LineSegment 2 p r
leftSide = (\(Corners t _ _ b) -> ClosedLineSegment b t) . corners

-- | The right side, oriented from *bottom* to top
rightSide :: Num r => Rectangle p r -> LineSegment 2 p r
rightSide = (\(Corners _ t b _) -> ClosedLineSegment b t) . corners


-- | The sides of the rectangle, in order (Top, Right, Bottom, Left). The sides
-- themselves are also oriented in clockwise order. If, you want them in the
-- same order as the functions `topSide`, `bottomSide`, `leftSide`, and
-- `rightSide`, use `sides'` instead.
sides   :: Num r => Rectangle p r -> Sides (LineSegment 2 p r)
sides r = let Corners nw ne se sw = corners r
          in Sides (ClosedLineSegment nw ne) (ClosedLineSegment ne se)
                   (ClosedLineSegment se sw) (ClosedLineSegment sw nw)

-- | The sides of the rectangle. The order of the segments is (Top, Right,
-- Bottom, Left).  Note that the segments themselves, are oriented as described
-- by the functions topSide, bottomSide, leftSide, rightSide (basically: from
-- left to right, and from bottom to top). If you want the segments oriented
-- along the boundary of the rectangle, use the `sides` function instead.
sides'   :: Num r => Rectangle p r -> Sides (LineSegment 2 p r)
sides' r = Sides (topSide r) (rightSide r) (bottomSide r) (leftSide r)
