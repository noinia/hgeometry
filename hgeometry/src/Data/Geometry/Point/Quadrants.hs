module Data.Geometry.Point.Quadrants where

import           Control.Lens
import           Data.Ext
import           Data.Geometry.Point.Class
import           Data.Geometry.Point.Internal
import           Data.Geometry.Vector
import qualified Data.List as L
import           GHC.TypeLits
import           Text.Read (Read(..))

--------------------------------------------------------------------------------

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
