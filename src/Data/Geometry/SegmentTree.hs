{-# LANGUAGE TemplateHaskell #-}
module Data.Geometry.SegmentTree( module Data.Geometry.SegmentTree.Generic
                                ) where


import           Data.Ext
import           Data.Semigroup
import           Control.Lens
import           Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.List as List
import           Data.BinaryTree
import           Data.Range
import           Data.Geometry.Interval
import           Data.Geometry.SegmentTree.Generic
import           Data.Geometry.Interval.Util
import           Data.Geometry.Properties
import           Data.Geometry.IntervalTree (IntervalLike(..))
