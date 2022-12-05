--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PolyLine
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- A Polyline and some basic functions to interact with them.
--
--------------------------------------------------------------------------------
module HGeometry.PolyLine
  ( PolyLineF(..), PolyLine
  , module HGeometry.PolyLine.Class
  ) where


import           Control.DeepSeq (NFData)
import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as NonEmpty
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import           GHC.Generics
import           HGeometry.Foldable.Util
import           HGeometry.Point
import           HGeometry.PolyLine.Class
import           HGeometry.Properties
import           HGeometry.Vector.NonEmpty.Util ()
import           Hiraffe.Graph

--------------------------------------------------------------------------------

-- | Simple polygons just store their vertices in CCCW order
newtype PolyLineF f point = MkPolylLine (f point)
  deriving (Generic)
  deriving newtype (NFData)

-- | By default we store simple poylline as non-empty vectors.
type PolyLine = SimplePolygonF NonEmptyVector
