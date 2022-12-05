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
  ) where

import Control.Lens
import Control.Lens.Internal.Fold (NonEmptyDList(..))
import Data.Functor.Apply (Apply)
import Data.Functor.Contravariant (phantom)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromMaybe)
import Data.Semigroup.Foldable
-- import HGeometry.LineSegment.Class
import HGeometry.Point.Class
import HGeometry.Vector.Class
import Hiraffe.Graph
import Data.Function (on)
import Data.Semigroup (First(..))

--------------------------------------------------------------------------------

-- | A class representing PolyLines
class ( HasVertices polyLine polyLine
      , Vertex polyLine ~ point
      , Point_ point d r
      ) => PolyLine_ polyLine d point r | polyLine -> point where
