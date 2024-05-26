--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PolyLine.Class
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Polyline class and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module HGeometry.PolyLine.Class
  ( PolyLine_(..)
  , ConstructablePolyLine_(..)
  , _PolyLineLineSegment
  ) where

import           Control.Lens
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Semigroup.Foldable
import           HGeometry.LineSegment.Class
import           HGeometry.Point.Class
import           HGeometry.Properties
import           Hiraffe.Graph

--------------------------------------------------------------------------------

-- | A class representing PolyLines
class ( HasVertices polyLine polyLine
      , HasPoints' polyLine point
      , HasStart polyLine point
      , HasEnd polyLine point
      , Vertex polyLine ~ point
      , Point_ point (Dimension point) (NumType point)
      , NumType polyLine ~ NumType point
      , Dimension polyLine ~ Dimension point
      ) => PolyLine_ polyLine point | polyLine -> point where

-- | Class for constructable polylglines
class PolyLine_ polyLine point => ConstructablePolyLine_ polyLine point where

  -- | Constructs a polyline from a given sequence of points.
  --
  -- pre: there should be at least two distinct points
  polyLineFromPoints :: Foldable1 f => f point -> polyLine


-- maybe make these two functions into a prism instead

-- | Prism between a polyline and a line segment
_PolyLineLineSegment :: ( ConstructableLineSegment_ lineSegment point
                        , ConstructablePolyLine_ polyLine point
                        ) => Prism' polyLine lineSegment
_PolyLineLineSegment = prism' lineSegmentToPolyLine polyLineToLineSegment
  where
    lineSegmentToPolyLine s = polyLineFromPoints . NonEmpty.fromList $ [s^.start, s^.end]

    polyLineToLineSegment pl
      | lengthOf vertices pl == 2 = Just $ uncheckedLineSegment (pl^.start) (pl^.end)
      | otherwise                 = Nothing
