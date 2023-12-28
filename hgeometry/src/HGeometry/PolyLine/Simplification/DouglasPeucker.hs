--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.PolyLine.Simplification.DouglasPeucker
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.PolyLine.Simplification.DouglasPeucker
  ( douglasPeucker
  ) where

import           Control.Lens hiding (only)
import           Data.Foldable1.WithIndex
import           Data.Function (on)
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Ord (comparing)
import           Data.Sequence (Seq(..))
import qualified Data.Sequence as Seq
import           HGeometry.LineSegment
import           HGeometry.Point
import           HGeometry.PolyLine
import           HGeometry.Sequence.NonEmpty

--------------------------------------------------------------------------------

-- | Line simplification with the well-known Douglas Peucker alogrithm. Given a distance
-- value eps and a polyline pl, constructs a simplification of pl (i.e. with
-- vertices from pl) s.t. all other vertices are within dist eps to the
-- original polyline.
--
-- Running time: \( O(n^2) \) worst case, \( O(n log n) \) on average.
douglasPeucker     :: forall polyLine point d r.
                      ( PolyLine_ polyLine point
                      , Point_ point d r
                      , Ord r, Fractional r
                      , HasSquaredEuclideanDistance point
                      )
                   => r -> polyLine -> PolyLineF ViewR1 point
douglasPeucker eps = go . polylineFromPoints . toNonEmptyOf allPoints
  where
    go pl
      | dst <= (eps*eps) = polylineFromPoints $ a :| [b]
      | otherwise        = go pref `merge` go subf
      where
        a, b :: point
        a             = pl^.start
        b             = pl^.end
        (OnSnd i dst) = maxDist (pl^._PolyLineF) (ClosedLineSegment a b)
        (pref,subf)   = split i pl

--------------------------------------------------------------------------------
-- * Internal functions

-- | Concatenate the two polylines, dropping their shared vertex
merge          :: PolyLineF ViewR1 point -> PolyLineF ViewR1 point -> PolyLineF ViewR1 point
merge pref sub = let (_ :<< sub') = sub^._PolyLineF.to viewl1
                 in PolyLine $ (pref^._PolyLineF) <>> sub'

-- | Split the polyline at the given vertex. Both polylines contain this vertex
split                         :: Int -> PolyLineF ViewR1 point
                              -> (PolyLineF ViewR1 point, PolyLineF ViewR1 point)
split i (PolyLine (vs :>> r)) = (PolyLine ls, PolyLine rs)
  where
    (ls',rs') = Seq.splitAt (i+1) vs
    (ls,rs) = case Seq.viewr ls' of
                (xs Seq.:> v) -> (xs :>> v, (v :<| rs') :>> r)
                _             -> error "DouglasPeucker, split: absurd"


-- | Given a sequence of points, find the index of the point that has the
-- Furthest distance to the LineSegment. The result is the index of the point
-- and this distance.
maxDist       :: ( Point_ point d r, Ord r, Fractional r, Foldable1WithIndex Int f
                 , HasSquaredEuclideanDistance point
                 )
              => f point -> ClosedLineSegment point -> OnSnd r
maxDist pts s = ifoldMap1' (\i p -> OnSnd i $ squaredEuclideanDistTo p s) pts

data OnSnd r = OnSnd { _getI :: {-# UNPACK #-}!Int, getR :: !r }

instance Eq r => Eq (OnSnd r) where
  (==) = (==) `on` getR
instance Ord r => Ord (OnSnd r) where
  compare = comparing getR
instance Ord r => Semigroup (OnSnd r) where
  a <> b | a < b     = b -- return the max
         | otherwise = a
