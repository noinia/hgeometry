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
  , lineSegmentToPolyLine
  ) where

import           Control.Lens
-- import Control.Lens.Internal.Fold (NonEmptyDList(..))
-- import Data.Functor.Apply (Apply)
-- import Data.Functor.Contravariant (phantom)
import qualified Data.List.NonEmpty as NonEmpty
-- import Data.Maybe (fromMaybe)
import           Data.Semigroup.Foldable
import           HGeometry.LineSegment.Class
import           HGeometry.Point.Class
import           HGeometry.Properties
-- import HGeometry.Vector.Class
import           Hiraffe.Graph
-- import Data.Function (on)
-- import Data.Semigroup (First(..))

--------------------------------------------------------------------------------

-- | A class representing PolyLines
class ( HasVertices polyLine polyLine
      , Vertex polyLine ~ point
      , Point_ point (Dimension point) (NumType point)
      ) => PolyLine_ polyLine point | polyLine -> point where

  -- | Constructs a polyline from a given sequence of points.
  --
  -- pre: there should be at least two distinct points
  polylineFromPoints :: Foldable1 f => f point -> polyLine

-- | Convert a line segment into a polyline
lineSegmentToPolyLine  :: ( LineSegment_ lineSegment point
                          , PolyLine_ polyLine point
                          ) => lineSegment -> polyLine
lineSegmentToPolyLine s = polylineFromPoints . NonEmpty.fromList $ [s^.start, s^.end]
