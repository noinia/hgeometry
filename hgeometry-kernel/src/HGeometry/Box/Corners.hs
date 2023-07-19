--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Box.Corners
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- The corners of a box.
--------------------------------------------------------------------------------
module HGeometry.Box.Corners
  ( Corners(Corners), northWest, northEast, southEast, southWest
  , corners, cornersInDirection
  ) where

import Control.Lens hiding ((<.>))
import Data.Foldable1
import Data.Functor.Apply
import GHC.Generics (Generic)
import HGeometry.Box.Class
import HGeometry.Direction
import HGeometry.Point
import HGeometry.Vector

--------------------------------------------------------------------------------

-- | A data type rperesenting the corners of a box.  the order of the
-- Corners is 'northWest, northEast, southEast, southWest', i.e. in
-- clockwise order starting from the topleft.
data Corners a = Corners { _northWest  :: !a
                         , _northEast  :: !a
                         , _southEast  :: !a
                         , _southWest  :: !a
                         }
               deriving stock (Show,Eq,Ord,Generic,Functor,Foldable,Traversable)

-- | Access the northwest corner
northWest :: Lens' (Corners a) a
northWest = lens _northWest (\c x -> c { _northWest = x})

-- | Access the northeast corner
northEast :: Lens' (Corners a) a
northEast = lens _northEast (\c x -> c { _northEast = x})

-- | Access the southwest corner
southWest :: Lens' (Corners a) a
southWest = lens _southWest (\c x -> c { _southWest = x})

-- | Access the southeast corner
southEast :: Lens' (Corners a) a
southEast = lens _southEast (\c x -> c { _southEast = x})

type instance Index   (Corners a) = InterCardinalDirection
type instance IxValue (Corners a) = a

instance Ixed (Corners a) where
  ix = \case
    NorthWest -> northWest
    NorthEast -> northEast
    SouthEast -> southEast
    SouthWest -> southWest

instance Foldable1 Corners where
  foldMap1 f (Corners a b c d) = f a <> f b <> f c <> f d
instance Traversable1 Corners where
  traverse1 f (Corners a b c d) = Corners <$> f a <.> f b <.> f c <.> f d

instance Applicative Corners where
  pure x = Corners x x x x
  (Corners f g h i) <*> (Corners a b c d) = Corners (f a) (g b) (h c) (i d)

instance Semigroup a => Semigroup (Corners a) where
  s <> s' = (<>) <$> s <*> s'
instance Monoid a => Monoid (Corners a) where
  mempty = pure mempty

--------------------------------------------------------------------------------

-- | Get the corners of a rectangle, the order is:
--
-- Any additional information from in the top points is taken from the Top point,
-- Any additional information from in the bottom points is taken from the Bottom point,
corners   :: ( Num r, Rectangle_ rectangle point, Point_ point 2 r
             -- , HasComponents (Vector 2 (ClosedInterval r)) (Vector 2 r)
             )
          => rectangle -> Corners point
corners r = let w = width r
                p = r^.minPoint
                q = r^.maxPoint
             in Corners (q&xCoord %~ subtract w) q (p&xCoord %~ (+ w)) p

--------------------------------------------------------------------------------

-- | Gets the corners in a particular direction
cornersInDirection     :: CardinalDirection -> Corners p -> Vector 2 p
cornersInDirection d c = (\icd -> c^?!ix icd) <$> interCardinalsOf d
