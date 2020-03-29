{-# LANGUAGE TemplateHaskell  #-}
module Data.Geometry.Box.Corners where

import Control.Lens (makeLenses,Ixed(..),Index, IxValue,(%~),(&))
import Data.Ext
import Data.Functor.Apply
import Data.Geometry.Box.Internal
import Data.Geometry.Point
import Data.Semigroup.Foldable.Class
import Data.Semigroup.Traversable.Class
import GHC.Generics (Generic)

--------------------------------------------------------------------------------

-- | A Quadrant data type
data Corners a = Corners { _northWest  :: !a
                         , _northEast  :: !a
                         , _southEast  :: !a
                         , _southWest  :: !a
                         } deriving (Show,Eq,Ord,Generic,Functor,Foldable,Traversable)
makeLenses ''Corners

-- | Data Type to index Corners
data Directions = NorthWest | NorthEast | SouthEast | SouthWest deriving (Show,Read,Eq,Ord,Generic)

type instance Index   (Corners a) = Directions
type instance IxValue (Corners a) = a

instance Ixed (Corners a) where
  ix = \case
    NorthWest -> northWest
    NorthEast -> northEast
    SouthEast -> southEast
    SouthWest -> southWest

instance Foldable1 Corners
instance Traversable1 Corners where
  traverse1 f (Corners a b c d) = Corners <$> f a <.> f b <.> f c <.> f d

instance Applicative Corners where
  pure x = Corners x x x x
  (Corners f g h i) <*> (Corners a b c d) = Corners (f a) (g b) (h c) (i d)

--------------------------------------------------------------------------------

-- | Get the corners of a rectangle, the order is:
-- (TopLeft, TopRight, BottomRight, BottomLeft).
-- The extra values in the Top points are taken from the Top point,
-- the extra values in the Bottom points are taken from the Bottom point
corners :: Num r => Rectangle p r -> Corners (Point 2 r :+ p)
corners r     = let w = width r
                    p = (_maxP r)&core %~ _cwMax
                    q = (_minP r)&core %~ _cwMin
                in Corners (p&core.xCoord %~ (subtract w)) p
                           (q&core.xCoord %~ (+ w))        q
