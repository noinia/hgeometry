--------------------------------------------------------------------------------
-- |
-- Module      :  HGeometry.ConvexHull.QuickHull
-- Copyright   :  (C) Frank Staals
-- License     :  see the LICENSE file
-- Maintainer  :  Frank Staals
--------------------------------------------------------------------------------
module HGeometry.ConvexHull.QuickHull
  ( convexHull
  ) where

import           Control.Lens
import qualified Data.Foldable as F
import qualified Data.List as List
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Ord (comparing)
import           HGeometry.Line
import           HGeometry.Point
import           HGeometry.Polygon.Convex
import           HGeometry.Polygon.Simple
import           HGeometry.Triangle
import           HGeometry.Intersection


import           HGeometry.Number.Real.Rational

-- import Data.Ratio
import qualified Data.List.NonEmpty as NonEmpty
import qualified HGeometry.ConvexHull.GrahamScan as GC
import Debug.Trace
--------------------------------------------------------------------------------

-- | ConvexHull using Quickhull. The resulting polygon is
-- given in clockwise order.
--
-- pre: input contains at least three points
--
-- running time: \(O(n^2)\)
convexHull            :: (Ord r, Fractional r, Show r, Point_ point 2 r)
                      => NonEmpty point -> ConvexPolygon point
convexHull ps        = uncheckedFromCCWPoints
                     $ [l] <> hull l r above <> [r] <> reverse (hull l r below)
  where
    Extremes l r mids = findExtremes ps
    m                 = lineThrough l r
    (above,below)     = List.partition (`liesAbove` m) mids

data Extremes p = Extremes !p !p [p] deriving (Show)

-- | Finds the leftmost and rightmost point in the list
findExtremes            :: (Ord r, Point_ point 2 r)
                        => NonEmpty point
                        -> Extremes point
findExtremes (p :| pts ) = foldr f (Extremes p p []) pts
  where
    f q (Extremes l r ms) = case (incXdecY q l, incXdecY q r) of
                              (LT,_)  -> Extremes q r (addIfNot r l ms)
                              (EQ,_)  -> Extremes l r ms -- ditch q; it is the same as l
                              (GT,GT) -> Extremes l q (addIfNot l r ms)
                              (GT,EQ) -> Extremes l r ms -- ditch q; it is the same as r
                              (GT,LT) -> Extremes l r (q:ms)

    addIfNot y x xs | (x^.asPoint) /= (y^.asPoint) = x:xs
                    | otherwise                    = xs

-- findExtremesBy         :: (a -> a -> Ordering)
--                        -> NonEmpty a
--                        -> STR a a [a]
-- findExtremesBy cmp pts = let l = F.minimumBy cmp pts
--                              r = F.maximumBy cmp pts
--                              a /=. b = a `cmp` b /= EQ
--                          in STR l r [p | p <- F.toList pts, p /=. l, p /=. r]


incXdecY :: (Ord r, Point_ point 2 r) => point -> point -> Ordering
incXdecY (Point2_ px py) (Point2_ qx qy) =
  compare px qx <> compare qy py

-- | include neigher left or right
--
hull         :: forall point r. (Fractional r, Ord r, Point_ point 2 r)
             => point -> point -> [point ] -> [point]
hull _ _ []  = []
hull l r pts = hull l mid ls <> [mid] <> hull mid r rs
  where
    m       :: LinePV 2 r
    m       = lineThrough l r
    mid     = F.maximumBy (comparing dist) pts

    dist p = p `squaredEuclideanDistTo` m
    t       = Triangle l mid r
    -- line through l and mid, which splits the remaining points in a left half and a right half.
    splitL   = lineThrough l mid
    rightSide = r `onSide` splitL -- define the side containing r the right side

    (ls,rs) = List.partition (\p -> p `onSide` splitL /= rightSide)
            . filter (\p -> not $ (p^.asPoint) `intersects` t) $ pts


-- mPoint2 [x,y] = Point2 x y

-- -- testPoints = NonEmpty.fromList
-- --   [ mPoint2 [22536303956634 % 7570647828779,(-5816376064439) % 1228319866920] :+ 1
-- --   , mPoint2 [(-3136920648983) % 824638230353,(-14583744643665) % 9604445576558] :+ 2
-- --   , mPoint2 [(-11653462784667) % 6525086575987,(-598434515815) % 1364557986096] :+ 3
-- --   , mPoint2 [(-7841595901661) % 3282967141364,(-207167076115) % 482378191549] :+ 4
-- --   ]


-- testPoints :: NonEmpty (Point 2 Rational :+ Int)
-- testPoints = read "(Point2 [(-11199966464450) % 1365514034959,4065659138075 % 2296468530516] :+ 1) :| [Point2 [86686001553073 % 2736621704548,(-63774454571048) % 1880665081093] :+ 2,Point2 [(-77322324895231) % 8260610289790,(-41165682123514) % 2291705829063] :+ 3,Point2 [2292642905947 % 2659329735076,87045289726355 % 2752214350419] :+ 4]"


-- toDouble          :: Point 2 Rational :+ a -> Point 2 Double :+ a
-- toDouble (p :+ x) = (realToFrac <$> p) :+ x


--        -- Falsified (after 36 tests and 4 shrinks):



-- testPoints2 :: NonEmpty (Point 2 Rational :+ Int)
-- testPoints2 = read "(Point2 [(-9876468593885) % 9254762894818,(-34982972348716) % 7450362538495] :+ 1) :| [Point2 [(-11974177119403) % 7705693443554,(-37634868551543) % 9311528788922] :+ 2,Point2 [(-32383659855458) % 9565531378857,20253950785876 % 8268868939819] :+ 3,Point2 [42425655100996 % 8786996213535,(-7972873491283) % 1604043452399] :+ 4]"

type R = RealNumber 5

testPts :: NonEmpty (Point 2 R)
testPts = Point2 0 0 :| [Point2 0.16719 (-0.50324),Point2 0.59524 0.09425]

test = convexHull testPts
gHull = GC.convexHull testPts


-- let t = Triangle l m r
-- (m^.asPoint) `intersects` t
-- the corners of a triangle should certainly intersect the triangle ..
