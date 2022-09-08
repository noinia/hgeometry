--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.Point.Quadrants
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.Point.Quadrants where

import           Control.Lens
import           HGeometry.Point.Class
import qualified Data.List as L

--------------------------------------------------------------------------------

-- | Quadrants of two dimensional points. in CCW order
data Quadrant = TopRight | TopLeft | BottomLeft | BottomRight
              deriving (Show,Read,Eq,Ord,Enum,Bounded)

-- | Quadrants around point c; quadrants are closed on their "previous"
-- boundary (i..e the boundary with the previous quadrant in the CCW order),
-- open on next boundary. The origin itself is assigned the topRight quadrant
quadrantWith     :: (Ord r, Point_ point 2 r)
                 => point -> point -> Quadrant
quadrantWith c p = case ( (c^.xCoord) `compare` (p^.xCoord)
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
quadrant :: (Ord r, Num r, Point_ point 2 r) => point -> Quadrant
quadrant = quadrantWith origin

-- | Given a center point c, and a set of points, partition the points into
-- quadrants around c (based on their x and y coordinates). The quadrants are
-- reported in the order topLeft, topRight, bottomLeft, bottomRight. The points
-- are in the same order as they were in the original input lists.
-- Points with the same x-or y coordinate as p, are "rounded" to above.
partitionIntoQuadrants       :: (Ord r, Point_ point 2 r)
                             => point
                             -> [point]
                             -> ( [point], [point]
                                , [point], [point]
                                )
partitionIntoQuadrants c pts = (topL, topR, bottomL, bottomR)
  where
    (below',above')   = L.partition (on yCoord) pts
    (bottomL,bottomR) = L.partition (on xCoord) below'
    (topL,topR)       = L.partition (on xCoord) above'

    on l q       = q^.l < c^.l
