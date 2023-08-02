--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.SegmentTree.R2
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--
-- Segment Tree specifically implemented for 2D segment stabbing/reporting/counting
-- queries.
--
--------------------------------------------------------------------------------
module HGeometry.SegmentTree.R2
  (

  ) where


import           Control.Lens
import           Data.Foldable1
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Monoid (Sum(..))
import           Data.Set (Set)
import qualified Data.Set as Set
import qualified Data.Vector as Vector
import           Data.Vector.NonEmpty.Internal (NonEmptyVector(..))
import           HGeometry.Foldable.Sort
import           HGeometry.Intersection
import           HGeometry.Interval
import           HGeometry.Measured.Size
import           HGeometry.Point
import           HGeometry.Properties
import qualified HGeometry.SegmentTree.Base as Base
import           HGeometry.Tree.Binary.Static
import           HGeometry.Vector.NonEmpty.Util ()

--------------------------------------------------------------------------------

newtype OnY segment = OnY segment

newtype WithY f segment = WithY (Set (OnY))


newtype SegmentTree2 f segment = SegmentTree2 (SegmentTree (WithY f) segment)
